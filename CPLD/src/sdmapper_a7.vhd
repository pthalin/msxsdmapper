--
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
use ieee.std_logic_misc.all;            -- AND_REDUCE

entity sdmapper is
  generic (
    dividirclock    : boolean := FALSE
  );
  port(
    
    clk12_i             : in    std_logic;
    
    --clock             : in    std_logic;
    reset_n             : in    std_logic;
    --dis_mapper          : in    std_logic;
    --mr_mp               : in    std_logic;  -- 1 = mapper, 0= megaram

    -- CPU interface
    addr_bus            : in    std_logic_vector(15 downto 0);
    data_bus            : inout std_logic_vector(7 downto 0);
    wr_n                : in    std_logic;
    rd_n                : in    std_logic;
    iorq_n              : in    std_logic;
    m1_n                : in    std_logic;
    sltsl_n             : in    std_logic;
    busdir_n            : out   std_logic;

    -- ROM interface
    --rom_a             : out   std_logic_vector(16 downto 14);
    --rom_cs            : out   std_logic;
    --rom_we            : out   std_logic;

    -- RAM interface
    ram_a_out       : out   std_logic_vector(18 downto 0);
    ram_d           : out   std_logic_vector(7 downto 0);
    ram_cs          : out   std_logic;
    ram_oe          : out   std_logic;
    ram_we          : out   std_logic;

    -- SD card interface
    --sd_cs         : out   std_logic_vector(1 downto 0);   -- Saida Chip Select para os cartoes
    sd_cs0          : out   std_logic;
    sd_sclk         : out   std_logic;                          -- Saida SCK
    sd_mosi         : out   std_logic;                          -- Master Out Slave In
    sd_miso         : in    std_logic;                          -- Master In Slave Out
    sd_writeprot    : in    std_logic_vector(1 downto 0);   -- 0 = cartao protegido contra escrita
    sd_inserted     : in    std_logic_vector(1 downto 0)    -- 0 = cartao inserido
  );

end sdmapper;

architecture Behavioral of sdmapper is

  signal io_cs            : std_logic;
  signal fw_en            : std_logic;
  signal iomapper         : std_logic;
  signal iomegaram        : std_logic;
  signal ffff             : std_logic;
  signal sltsl_c          : std_logic;
  signal slt_exp_n        : std_logic_vector(3 downto 0);
  signal sltsl_rom        : std_logic;
  signal sltsl_ram        : std_logic;

  -- Porta SPI
  signal sd_en            : std_logic;        -- '0' acessa ROM, '1' acessa porta SPI (4000-42FF e 4800)
  signal sd_addr          : std_logic;
  signal sd_pcs           : std_logic;
  signal sd_chav          : std_logic;
  signal clock_sd         : std_logic;

  -- flash
  signal rom_chav         : std_logic;
  signal mr_addr          : std_logic_vector(2 downto 0);
  signal rom_rd1          : std_logic;
  signal rom_rd2          : std_logic;
  signal rom_rd           : std_logic;
  signal rom_wr           : std_logic;
  signal rom_wr_en        : std_logic;
  signal rom_adhi         : std_logic_vector(2 downto 0);

  -- added
  signal rom_data         : std_logic_vector(7 downto 0);
  signal ram_a            : std_logic_vector(18 downto 13);
  signal rom_cs           : std_logic;
  signal clock            : std_logic;
  signal clk100           : std_logic;
  signal sd_cs            : std_logic_vector(1 downto 0);

  -- Not impl.
  signal rom_a            : std_logic_vector(16 downto 14);
  signal rom_we           : std_logic;

 signal dis_mapper          : std_logic;  -- 1 = disable mapper?
 signal mr_mp               : std_logic;  -- 1 = mapper, 0= megaram


  COMPONENT ROM
  PORT (
    clka  : IN  STD_LOGIC;
    addra : IN  STD_LOGIC_VECTOR(13 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
  END COMPONENT;
  
  COMPONENT clk_wiz_0
  Port ( 
    clk25   : out STD_LOGIC;
    clk100  : out STD_LOGIC;
    clk_in1 : in  STD_LOGIC
  );
  END COMPONENT;
       
  COMPONENT dist_mem_gen_0
  PORT (
    a   : IN  STD_LOGIC_VECTOR(13 DOWNTO 0);
    spo : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
  END COMPONENT;
begin


  dis_mapper  <= '0';
  mr_mp       <= '1';

  sd_cs0 <= sd_cs(0);

  ram_a_out <= ram_a & addr_bus(12 downto 0); 
  
  rom_inst : dist_mem_gen_0
  PORT MAP (
    a   => addr_bus(13 DOWNTO 0), --add 0x4000 ta addr.
    spo => rom_data
  ); 
  data_bus <= rom_data when rom_cs = '1' and rd_n = '0'  else   (others => 'Z');

  clk_wiz_o_inst: clk_wiz_0
  Port MAP ( 
    clk25  => clock,
    clk100 => clk100,
    clk_in1=> clk12_i
  );

  -- Porta SPI
  portaspi: entity work.sd
  port map (
    clock           => clock_sd,
    reset_n         => reset_n,
    dis_mapper      => dis_mapper,
    mr_mp           => mr_mp,
    -- CPU interface
    cs              => sd_pcs,
    addr_bus        => sd_addr,
    data_bus        => data_bus,
    wr_n            => wr_n,
    rd_n            => rd_n,
    -- SD card interface
    sd_cs           => sd_cs,
    sd_sclk         => sd_sclk,
    sd_mosi         => sd_mosi,
    sd_miso         => sd_miso,
    sd_writeprot    => sd_writeprot,
    sd_inserted     => sd_inserted
  );

  -- Expansor de slot
  exp: entity work.exp_slot
  port map (
    reset_n     => reset_n,
    sltsl_n     => sltsl_c,
    cpu_rd_n    => rd_n,
    cpu_wr_n    => wr_n,
    ffff        => ffff,
    cpu_a       => addr_bus(15 downto 14),
    cpu_d       => data_bus,
    exp_n       => slt_exp_n
  );

  -- Mapper
  mpmr: entity work.megamapper
  port map (
    reset_n     => reset_n,
    cpu_a       => addr_bus,
    cpu_d       => data_bus,
    mr_mp       => mr_mp,
    ioFx        => iomapper,
    io8x        => iomegaram,
    cpu_rd_n    => rd_n,
    cpu_wr_n    => wr_n,
    sltsl_n     => sltsl_ram,
    sram_ma     => ram_a,
    sram_cs_n   => ram_cs,
    sram_we_n   => ram_we,
    busdir_n    => busdir_n
  );

  -- Glue Logic

  -- FFFF is 1 when all bits of the address bus is 1 (used in the slot expander)
  ffff    <= AND_REDUCE(addr_bus);

  -- Slot Selects
  sltsl_c     <= sltsl_n      when dis_mapper = '1' else '1';
  sltsl_rom   <= slt_exp_n(1) when dis_mapper = '1' else sltsl_n;
  sltsl_ram   <= slt_exp_n(3) when dis_mapper = '1' else '1';

  -- Enable for I/O registers
  io_cs           <= not iorq_n and m1_n;
  fw_en           <= '1' when io_cs = '1' and addr_bus(7 downto 0) = X"5F" and wr_n = '0' else '0';   -- Writing I/O register $5F
  iomapper        <= '1' when io_cs = '1' and addr_bus(7 downto 2) = "111111"             else '0';   -- Reand/write I/O registers $FC to $FF
  iomegaram       <= '1' when io_cs = '1' and addr_bus(7 downto 1) = "1000111"            else '0';   -- Reand/write I/O registers $8E to $8F

  -- Write register $B7
  process (reset_n, fw_en)
  begin
    if reset_n = '0' then
      rom_wr_en <= '0';
      rom_adhi  <= (others => '0');
    elsif falling_edge(fw_en) then
      rom_wr_en   <= data_bus(7);
      rom_adhi    <= data_bus(2 downto 0);
    end if;
  end process;

  -- Control of Nextor MEGAROM ASCII16
  -- Writing the address $ 6000 selects the ROM bank if it is not in flash writing mode
  rom_chav <= '1' when wr_n = '0' and sltsl_rom = '0' and addr_bus = X"6000" and rom_wr_en = '0'  else '0';
  -- Wriing the address $ 6001 select between ROM and SPI
  sd_chav <= '1'  when wr_n = '0' and sltsl_rom = '0' and addr_bus = X"6001" and rom_wr_en = '0'  else '0';

  process (reset_n, rom_chav)
  begin
    if reset_n = '0' then 
      mr_addr <= (OTHERS => '0');
    elsif falling_edge(rom_chav) then
      mr_addr <= data_bus(2 downto 0);
    end if;
  end process;

  --  ROM accessed for reading on page 1 or 2 or writing on any page
  rom_rd1 <= '1'  when sltsl_rom = '0' and rd_n = '0' and addr_bus(15 downto 14) = "01"   else '0';
  rom_rd2 <= '1'  when sltsl_rom = '0' and rd_n = '0' and addr_bus(15 downto 14) = "10"   else '0';
  rom_wr  <= '1'  when sltsl_rom = '0' and wr_n = '0' and rom_wr_en = '1'                     else '0';
  rom_rd  <= rom_rd1 or rom_rd2;

  -- ROM address control
  rom_a <= rom_adhi           when rom_wr_en = '1'               and sltsl_rom = '0'  else    -- Generates address for FLASH writing
           mr_addr            when addr_bus(15 downto 14) = "01" and sltsl_rom = '0'  else    -- Generate Nextor address only on page 1 (4000-7FFF)
           (others => '-');

  -- Control of ROM /CE  
  rom_cs <= '0' when rom_wr = '1' and sd_en = '0'     else    -- Flash Writing (if SPI is off)
            '0' when rom_rd = '1' and sd_en = '0'     else    -- Flash Reading (if SPI is off)
            '0' when rom_rd = '1' and sd_pcs = '0'    else    -- Flash Reading (if SPI is connected and not addressed)
            '1';

  -- Control of ROM /WR
  rom_we <= '0' when rom_wr = '1' and sd_en = '0'     else    -- Write signal when enabled and SPI off
            '1';

  process (reset_n, sd_chav)
  begin
    if reset_n = '0' then 
      sd_en       <= '0';
    elsif falling_edge(sd_chav) then
      sd_en       <= data_bus(0);
    end if;
  end process;

  sd_pcs  <= '1'  when sd_en = '1' and sltsl_rom = '0' and rom_wr_en = '0' and
                       (addr_bus(15 downto 11) = "01000" or addr_bus = X"4800")
                  else
             '0';

  sd_addr <= not addr_bus(11);    --  2^11 = 0x400 => differentiate $4000-47FF and $4800

  divclk: if dividirclock generate
    process (reset_n, clock)
    begin
      if reset_n = '0' then
        clock_sd <= '0';
      elsif rising_edge(clock) then
        clock_sd <= not clock_sd;
      end if;
    end process;
  end generate;
  ndivclk: if not dividirclock generate
    clock_sd <= clock;
  end generate;

end Behavioral;
