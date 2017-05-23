
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi is
	port(
		clock_i			: in    std_logic;
		reset_n_i		: in    std_logic;
		flags_i			: in    std_logic_vector(7 downto 0);
		-- CPU interface
		cs_i				: in    std_logic;
		addr_bus_i		: in    std_logic;
		data_bus_io		: inout std_logic_vector(7 downto 0);
		wr_n_i			: in    std_logic;
		rd_n_i			: in    std_logic;
		ctrl_rd_o		: out   std_logic;
		-- SD card interface
		sd_cs_n_o		: out   std_logic_vector(1 downto 0);
		sd_sclk_o		: out   std_logic;
		sd_mosi_o		: out   std_logic;
		sd_miso_i		: in    std_logic
	);

end entity;

architecture Behavioral of spi is

	signal spi_cs_s			: std_logic;
	signal port0_wr_s			: std_logic;
	signal port1_cs_s			: std_logic;
	signal spi_data_q			: std_logic_vector(7 downto 0);
	signal status_s			: std_logic_vector(7 downto 0);
	-- State type of the SPI transfer state machine
	type   state_type_t is (s_idle, s_running, s_done);
	signal state_s				: state_type_t;
	signal shift_reg_s		: std_logic_vector(7 downto 0);	-- Shift register
	signal spi_data_buf_s	: std_logic_vector(7 downto 0);	-- Buffer to hold data to be sent
	signal start_s				: std_logic;							-- Start transmission flag
	signal count_q				: unsigned(3 downto 0);				-- Number of bits transfered
	signal spi_clk_buf_s		: std_logic;							-- Buffered SPI clock
	signal spi_clk_out_s		: std_logic;							-- Buffered SPI clock output
	signal prev_spi_clk_s	: std_logic;							-- Previous SPI clock state
	signal ff_q, ff_clr_s	: std_logic;

begin

	-- flip-flop
	process(ff_clr_s, clock_i)
	begin
		if ff_clr_s = '1' then
			ff_q	<= '0';
		elsif rising_edge(clock_i) then
			ff_q	<= start_s;
		end if;
	end process;

	-- Enable portas
	spi_cs_s		<= '1'	when cs_i = '1' and (wr_n_i = '0' or rd_n_i = '0')		else '0';
	port0_wr_s	<= '1'	when spi_cs_s = '1' and wr_n_i = '0' and addr_bus_i = '0'	else '0';
	port1_cs_s	<= '1'	when spi_cs_s = '1'                  and addr_bus_i = '1'	else '0';

	-- Leitura das portas
	data_bus_io <= status_s		when spi_cs_s = '1' and rd_n_i = '0' and addr_bus_i = '0'	else
						spi_data_q	when spi_cs_s = '1' and rd_n_i = '0' and addr_bus_i = '1'	else
						(others => 'Z');

	ctrl_rd_o <= '1' when spi_cs_s = '1' and rd_n_i = '0' and addr_bus_i = '0'	else '0';

	-- Escrita porta 0
	process(reset_n_i, port0_wr_s)
	begin
		if reset_n_i = '0' then
			sd_cs_n_o <= "11";
		elsif falling_edge(port0_wr_s) then
			sd_cs_n_o <= data_bus_io(1 downto 0);
		end if;
	end process;

	-- Acesso porta 1
	process (reset_n_i, ff_clr_s, port1_cs_s)
	begin
		if reset_n_i = '0' or ff_clr_s = '1' then
			spi_data_buf_s	<= (others => '1');
			start_s			<= '0';
		elsif rising_edge(port1_cs_s) then					-- funciona somente se rising_edge
			if rd_n_i = '0' then
				spi_data_buf_s <= (others => '1');
			else
				spi_data_buf_s <= data_bus_io;
			end if;
			start_s <= '1';
		end if;
	end process;

	status_s <= flags_i;

	--------------------------------------------------
	-- Essa parte lida com a porta SPI por hardware --
	--      Implementa um SPI Master Mode 0         --
	--------------------------------------------------

	-- SPI write
	process(clock_i, reset_n_i)
	begin		
		if reset_n_i = '0' then
			ff_clr_s <= '0';
		elsif rising_edge(clock_i) then

			prev_spi_clk_s <= spi_clk_buf_s;
			case state_s is

				when s_idle =>
					if ff_q = '1' then
						count_q     <= (others => '0');
						shift_reg_s <= spi_data_buf_s;
						state_s     <= s_running;
					end if;

				when s_running =>
					if prev_spi_clk_s = '1' and spi_clk_buf_s = '0' then
						spi_clk_out_s <= '0';
						count_q       <= count_q + 1;
						shift_reg_s   <= shift_reg_s(6 downto 0) & sd_miso_i;
						if count_q = "0111" then
							state_s		<= s_done;
							ff_clr_s		<= '1';
						end if;
					elsif prev_spi_clk_s = '0' and spi_clk_buf_s = '1' then
						spi_clk_out_s <= '1';
					end if;

				when s_done =>
					spi_data_q	<= shift_reg_s;
					state_s		<= s_idle;
					ff_clr_s		<= '0';
				when others =>
					null;
			end case;
		end if;
	end process;

	-- Generate SPI clock
	spi_clock_gen : process(clock_i, reset_n_i)
	begin
		if reset_n_i = '0' then
			spi_clk_buf_s   <= '0';
		elsif rising_edge(clock_i) then
			if state_s = s_running then
				spi_clk_buf_s <= not spi_clk_buf_s;
			else
				spi_clk_buf_s <= '0';
			end if;
		end if;
	end process;

	sd_mosi_o <= shift_reg_s(7);
	sd_sclk_o <= spi_clk_out_s;

end architecture;
