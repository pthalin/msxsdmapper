--
-- Projeto MSX SD Mapper
--
-- Copyright (c) 2014
-- Fabio Belavenuto

-- This documentation describes Open Hardware and is licensed under the CERN OHL v. 1.1.
-- You may redistribute and modify this documentation under the terms of the
-- CERN OHL v.1.1. (http://ohwr.org/cernohl). This documentation is distributed
-- WITHOUT ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING OF MERCHANTABILITY,
-- SATISFACTORY QUALITY AND FITNESS FOR A PARTICULAR PURPOSE.
-- Please see the CERN OHL v.1.1 for applicable conditions

-- Modulo TOP - implementa o controle da Megarom ASCII16 para o Nextor

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sdmapper is
	port(
		clock_i			: in    std_logic;
		reset_n_i		: in    std_logic;
		dis_mapper_i	: in    std_logic;								-- 1 = enabled
		mr_mp_i			: in    std_logic;								-- 1 = mapper
		-- BUS interface
		addr_bus_i		: in    std_logic_vector(15 downto 0);
		data_bus_io		: inout std_logic_vector( 7 downto 0);
		wr_n_i			: in    std_logic;
		rd_n_i			: in    std_logic;
		iorq_n_i			: in    std_logic;
		m1_n_i			: in    std_logic;
		sltsl_n_i		: in    std_logic;
		busdir_n_o		: out   std_logic;
		-- ROM interface
		rom_a_o			: out   std_logic_vector(16 downto 14);
		rom_ce_n_o		: out   std_logic;
		rom_we_n_o		: out   std_logic;
		-- RAM interface
		ram_a_o			: out   std_logic_vector(18 downto 13);
		ram_cs_o			: out   std_logic;
		ram_we_o			: out   std_logic;
		-- SD card interface
		sd_cs_n_o		: out   std_logic_vector( 1 downto 0);
		sd_sclk_o		: out   std_logic;
		sd_mosi_o		: out   std_logic;
		sd_miso_i		: in    std_logic;
		sd_wp_n_i		: in    std_logic_vector( 1 downto 0);		-- 0 = Write protected
		sd_pres_n_i		: in    std_logic_vector( 1 downto 0)		-- 0 = SD Card present
	);

end sdmapper;

architecture Behavioral of sdmapper is

	signal io_cs	  		: std_logic;
	signal iomapper_s		: std_logic;
	signal iomegaram_s	: std_logic;
	signal ffff				: std_logic;
	signal sltsl_c			: std_logic;
	signal slt_exp_n		: std_logic_vector(3 downto 0);
	signal sltsl_rom_n_s	: std_logic;
	signal sltsl_ram_n_s	: std_logic;

	-- SPI port
	signal spi_cs_s		: std_logic;
	signal sd_chg_q		: std_logic_vector(1 downto 0);
	signal status_s		: std_logic_vector(7 downto 0);
	signal spi_ctrl_wr_s	: std_logic;
	signal spi_ctrl_rd_s	: std_logic;
	signal spi_mode_wr_s	: std_logic;
	signal spi_mode_q		: std_logic;

	-- Flash ASCII16
	signal rom_bank_wr_s	: std_logic;
	signal rom_bank1_q	: std_logic_vector(2 downto 0);
	signal rom_bank2_q	: std_logic_vector(2 downto 0);

begin

	-- Porta SPI
	portaspi: entity work.spi
	port map (
		clock_i			=> clock_i,
		reset_n_i		=> reset_n_i,
		-- CPU interface
		cs_i				=> spi_cs_s,
		data_bus_io		=> data_bus_io,
		wr_n_i			=> wr_n_i,
		rd_n_i			=> rd_n_i,
		-- SD card interface
		spi_sclk_o		=> sd_sclk_o,
		spi_mosi_o		=> sd_mosi_o,
		spi_miso_i		=> sd_miso_i
	);

	-- Expansor de slot
	exp: entity work.exp_slot
	port map (
		reset_n		=> reset_n_i,
		sltsl_n		=> sltsl_c,
		cpu_rd_n		=> rd_n_i,
		cpu_wr_n		=> wr_n_i,
		ffff			=> ffff,
		cpu_a			=> addr_bus_i(15 downto 14),
		cpu_d			=> data_bus_io,
		exp_n			=> slt_exp_n
	);

	-- Mapper
	mpmr: entity work.megamapper
	port map (
		reset_n		=> reset_n_i,
		cpu_a			=> addr_bus_i,
		cpu_d			=> data_bus_io,
		mr_mp			=> mr_mp_i,
		ioFx			=> iomapper_s,
		io8x			=> iomegaram_s,
		cpu_rd_n		=> rd_n_i,
		cpu_wr_n		=> wr_n_i,
		sltsl_n		=> sltsl_ram_n_s,
		sram_ma		=> ram_a_o,
		sram_cs_n	=> ram_cs_o,
		sram_we_n	=> ram_we_o,
		busdir_n		=> busdir_n_o
	);

	-- Glue Logic

	-- Enable portas I/O
	io_cs			<= not iorq_n_i and m1_n_i;

	-- Slot expander address select
	ffff    <= '1' when addr_bus_i = X"FFFF" else '0';

	-- Slot Selects
	sltsl_c			<= sltsl_n_i    when dis_mapper_i = '1' else '1';
	sltsl_rom_n_s	<= slt_exp_n(0) when dis_mapper_i = '1' else sltsl_n_i;
	sltsl_ram_n_s	<= slt_exp_n(1) when dis_mapper_i = '1' else '1';

	iomapper_s	<= '1' when io_cs = '1' and addr_bus_i(7 downto 2) = "111111"				else '0';	-- Acesso I/O portas $FC a $FF
	iomegaram_s	<= '1' when io_cs = '1' and addr_bus_i(7 downto 1) = "1000111"				else '0';	-- Acesso I/O portas $8E a $8F

	-- Status flags
	-- b0 : 1=SD Card on slot-0 changed since last read
	-- b1 : 1=SD Card on slot-1 changed since last read
	-- b2 : 0=SD card present on slot-0
	-- b3 : 0=SD card present on slot-1
	-- b4 : 0=Write protecton enabled for SD card slot-0
	-- b5 : 0=Write protecton enabled for SD card slot-1
	-- b6 : SW0 status. 0=RAM disabled, 1=RAM enabled
	-- b7 : SW1 status. 0=RAM mode: MegaRAM, 1=RAM mode: Memory Mapper

--	status_s	<= mr_mp_i & dis_mapper_i & sd_wp_n_i & sd_pres_n_i & sd_chg_q;

	-- Old fashion
	--;	b0	: 0=SD card present on slot-0
	--;	b1	: 0=SD card present on slot-1
	--;	b2	: 0=Write protecton enabled for SD card slot-0
	--;	b3	: 0=Write protecton enabled for SD card slot-1
	--;	b4	: SW0 status. 0=RAM enabled, 1=RAM disabled
	--;	b5	: SW1 status. 0=RAM mode: MegaRAM, 1=RAM mode: Memory Mapper
	--;	b6	: Reserved for future use. Must be masked out from readings.
	--;	b7	: 1=SPI transfer busy. 0=No ongoing SPI transfer

	status_s	<= "00" & mr_mp_i & dis_mapper_i & sd_wp_n_i & sd_pres_n_i;

	-- Megarom ASCII16
	-- 6000 = 011 00...
	-- 6800 = 011 01...
	-- 7000 = 011 10...
	-- 7800 = 011 11...

	rom_bank_wr_s	<= '1' when sltsl_n_i = '0' and wr_n_i = '0' and addr_bus_i(15 downto 13) = "011"	else '0';	-- 6000-7FFF

	-- Bank write
	process (reset_n_i, rom_bank_wr_s)
	begin
		if reset_n_i = '0' then
			rom_bank1_q		<= "000";
			rom_bank2_q		<= "001";
		elsif falling_edge(rom_bank_wr_s) then
			case addr_bus_i(12 downto 11) is
				when "00"   =>
					rom_bank1_q		<= data_bus_io(2 downto 0);
				when "10"   =>
					rom_bank2_q		<= data_bus_io(2 downto 0);
				when others =>
					null;
			end case;
		end if;
	end process;

	-- Flash control
	rom_a_o <= 
		rom_bank1_q 	when addr_bus_i(15 downto 14) = "01" and sltsl_rom_n_s = '0'	else
		rom_bank2_q		when addr_bus_i(15 downto 14) = "10" and sltsl_rom_n_s = '0'	else
		(others => '-');

	-- Flash /CS control
	rom_ce_n_o <=
		-- Excludes SPI range
		'0'	when addr_bus_i(15 downto 14) = "01" and sltsl_rom_n_s = '0' and rd_n_i = '0'	and spi_cs_s = '0'	else
		'0'	when addr_bus_i(15 downto 14) = "10" and sltsl_rom_n_s = '0'													else
		'1';

	-- Flash /WE control
	rom_we_n_o	<=	'0'	when addr_bus_i(15 downto 14) = "10" and sltsl_rom_n_s = '0' and wr_n_i = '0'	else
						'1';

	-- Disk change FFs
	process (reset_n_i, spi_ctrl_rd_s, sd_pres_n_i(0))
	begin
		if reset_n_i = '0' or spi_ctrl_rd_s = '1' then
			sd_chg_q(0) <= '0';
		elsif falling_edge(sd_pres_n_i(0)) then
			sd_chg_q(0) <= '1';
		end if;
	end process;

	process (reset_n_i, spi_ctrl_rd_s, sd_pres_n_i(1))
	begin
		if reset_n_i = '0' or spi_ctrl_rd_s = '1' then
			sd_chg_q(1) <= '0';
		elsif falling_edge(sd_pres_n_i(1)) then
			sd_chg_q(1) <= '1';
		end if;
	end process;

	-- SPI
	spi_ctrl_wr_s <= '1' when sltsl_rom_n_s = '0' and wr_n_i = '0' and rom_bank1_q = "111" and addr_bus_i = X"7F00"	else '0';
	spi_ctrl_rd_s <= '1' when sltsl_rom_n_s = '0' and rd_n_i = '0' and rom_bank1_q = "111" and addr_bus_i = X"7F00"	else '0';
	spi_mode_wr_s <= '1' when sltsl_rom_n_s = '0' and wr_n_i = '0' and rom_bank1_q = "111" and addr_bus_i = X"7F01"	else '0';

	-- SPI Control register write
	process (reset_n_i, spi_ctrl_wr_s)
	begin
		if reset_n_i = '0' then
			sd_cs_n_o	<= "11";
		elsif falling_edge(spi_ctrl_wr_s) then
			sd_cs_n_o	<= data_bus_io(1 downto 0);
		end if;
	end process;

	-- SPI Mode register write
	process (reset_n_i, spi_mode_wr_s)
	begin
		if reset_n_i = '0' then
			spi_mode_q	<= '0';
		elsif falling_edge(spi_mode_wr_s) then
			spi_mode_q	<= data_bus_io(0);
		end if;
	end process;


	-- 7B00 = 0111 1011
	-- 7C00 = 0111 1100
	-- 7D00 = 0111 1101
	-- 7E00 = 0111 1110
	-- 7F00 = 0111 1111

	spi_cs_s	<= '1'  when spi_mode_q = '1' and sltsl_rom_n_s = '0' and rom_bank1_q = "111" and addr_bus_i >= X"7B00" and addr_bus_i <= X"7EFF"   else
	            '0';

	-- Bus
	data_bus_io	<= status_s	when spi_ctrl_rd_s = '1'	else
						(others => 'Z');

end Behavioral;
