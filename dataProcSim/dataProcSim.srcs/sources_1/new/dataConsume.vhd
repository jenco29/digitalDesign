library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.math_real.ALL;
USE ieee.numeric_std.ALL;
use work.common_pack.all;

entity dataConsume is
  Port (
  	  clk:		in std_logic;
		reset:		in std_logic; -- synchronous reset
		start: in std_logic; -- goes high to signal data transfer
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
		ctrlIn: in std_logic;
		ctrlOut: out std_logic;
		data: in std_logic_vector(7 downto 0);
		dataReady: out std_logic;
		byte: out std_logic_vector(7 downto 0);
		seqDone: out std_logic;
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0);
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1)
   );
end dataConsume;

architecture asm of dataConsume is
type state_type is (wait_start, check_index, req_data, new_data, read_data, compare, new_max, end_data, send_bytes);
signal curState, nextState: state_type;

signal index, max_index, num_words_reg: integer range 0 to SEQ_LENGTH;
signal data_reg: std_logic_vector(7 downto 0);
signal ctrl_out_reg, ctrlIn_delayed, ctrlIn_detected, read_done, dataReady_reg : std_logic;
signal data_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH);

begin

reg_num_words : process(clk)
variable i : integer range 0 to 2;
begin
    for i in 0 to 2 loop
        num_words_reg <= num_words_reg + to_integer(signed(numWords_bcd(i)) * (10 ** (3-i)));
    end loop;
end process;

reg_data : process(clk)
begin
    if rising_edge(clk) then
        data_reg <= data;
    end if;
end process;  

delay_CtrlIn: process(clk)     
begin
  if rising_edge(clk) then
    ctrlIn_delayed <= ctrlIn;
  end if;
end process;
  
ctrlIn_detected <= ctrlIn xor ctrlIn_delayed;

bytes_out : process(clk, dataReady_reg)
begin
    if read_done = '1' and rising_edge(clk) then
        if dataReady_reg = '0' then
            dataReady <= '1';
            dataReady_reg <= '1';
        else
            dataReady <= '0';
            dataReady_reg <= '0';
            byte <= data;
        end if;
    end if;
end process;

state_logic : process(curState)
begin
    case curState is
        when req_data =>
            ctrl_out_reg <= not ctrl_out_reg;
            ctrlOut <= ctrl_out_reg;
            index <= index + 1;
        
        when read_data => 
            data_store(index) <= data_reg;
        
        when new_max =>
            max_index <= index;
            
        when end_data =>
            read_done <= '1';
            
        when send_bytes =>
            
        when others =>
            -- do nothing
    end case;
end process;

next_state_comb : process(curState)
begin
    case curState is 
        when wait_start =>
            if start = '1' then
                nextState <= req_data;
            else
                nextState <= wait_start;
            end if;
            
        when req_data =>
            nextState <= new_data;
            
        when new_data =>
            if ctrlIn_detected = '1' then
                nextState <= check_index;
            else 
                nextState <= new_data;
            end if;
            
        when check_index =>
            if index < num_words_reg then
                nextState <= read_data;
            else
                nextState <= end_data;
            end if;
            
        when read_data =>
            nextState <= check_index;
                
        when compare =>
            if data_store(index) > data_store(max_index) then
                nextState <= new_max;
            else
                nextState <= req_data;
            end if;
        
        when end_data =>
            nextState <= send_bytes;
    end case;
end process;    
    
next_state_seq : process(clk, reset)
begin
    if reset = '1' then
        curState <= wait_start;
        index <= -1;
    elsif rising_edge(clk) then
        curState <= nextState;
    end if;
end process;        


end asm;
