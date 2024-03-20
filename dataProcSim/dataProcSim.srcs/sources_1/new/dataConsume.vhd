library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.math_real.ALL;
use ieee.numeric_std.ALL;
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
type state_type is (wait_start, check_index, req_data, new_data, read_data, compare, new_max, end_data, set_low1, set_low2, set_low3, set_max, set_high1, set_high2, set_high3, send_results);
signal curState, nextState: state_type;

-- index: position of data point currently being compared
-- max_index: position of largest currently known data point
-- num_words_reg: stores desired number of words from input
signal index, max_index, max_index_conv, num_words_reg: integer range 0 to SEQ_LENGTH -1;
-- data_reg: register to synchronously store current data point
signal data_reg: std_logic_vector(7 downto 0);
-- ctrl_out_reg: stores current value of output to data generator, used for toggling
-- ctrlIn_delayed, ctrlIn_detected: values used in logic for finding change in signal from data generator
-- read_done: 
-- dataReady_reg: 
signal ctrl_out_reg, ctrlIn_delayed, ctrlIn_detected, read_done, dataReady_reg : std_logic;
-- data_store: register that stores full sequence of data points from data generator
signal data_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH -1);
signal data_results_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
signal max_ind_reg : BCD_ARRAY_TYPE(0 to 2);
type INT_ARRAY is array (integer range<>) of integer;

begin

--turning numWordsBCD into an integer
bcd_to_int : process(clk)
variable i : integer range 0 to 2;
begin
    for i in 0 to 2 loop
        num_words_reg <= num_words_reg + to_integer(signed(numWords_bcd(i)) * (10 ** (3-i)));
    end loop;
end process;

int_to_bcd : process(clk)
variable bcd : BCD_ARRAY_TYPE(2 downto 0);
variable bits : INT_ARRAY(2 downto 0);
variable i : integer range 0 to 2;
begin
    max_index_conv <= max_index;
    for i in 2 to 0 loop
       bits(i) := max_index_conv mod 10;
       bcd(i) := std_logic_vector(to_signed(bits(i), BCD_WORD_LENGTH-1));
       max_index_conv <= (max_index_conv - bits(i))/10; 
    end loop;
    max_ind_reg <= bcd;
end process;

--storing data value inputted on the clock edge
reg_data : process(clk)
begin
    if rising_edge(clk) then
        data_reg <= data;
    end if;
end process;  

--setting ctrlIn_detected to signal change in input signal
delay_CtrlIn: process(clk)     
begin
  if rising_edge(clk) then
    ctrlIn_delayed <= ctrlIn;
  end if;
end process;
  
ctrlIn_detected <= ctrlIn xor ctrlIn_delayed;

--processes on asm states
state_logic : process(curState, reset)
begin
    if reset = '1' then
        index <= -1;
    end if;
    
    case curState is
        when req_data =>
            ctrl_out_reg <= not ctrl_out_reg;
            ctrlOut <= ctrl_out_reg;
            index <= index + 1;
        
        when read_data => 
            data_store(index) <= data_reg;
            dataReady <= '1';
            byte <= data_reg;
        
        when compare =>
            dataReady <= '0';
        
        when new_max =>
            max_index <= index;
            
        when end_data =>
            read_done <= '1';
            
        when set_low1 =>
            data_results_reg(0) <= data_store(max_index -3);
        
        when set_low2 =>
            data_results_reg(1) <= data_store(max_index -2);
        
        when set_low3 =>
            data_results_reg(2) <= data_store(max_index -1);
        
        when set_max =>
            data_results_reg(3) <= data_store(max_index);
        
        when set_high1 =>
            data_results_reg(4) <= data_store(max_index +1);
            
        when set_high2 =>
            data_results_reg(5) <= data_store(max_index +2);
            
        when set_high3 =>
            data_results_reg(6) <= data_store(max_index +3);
            seqDone <= '1';
            
        when send_results =>
            dataResults <= data_results_reg;
            maxIndex <= max_ind_reg;
               
        when others =>
            -- do nothing
    end case;
end process;

--changing asm states
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
            nextState <= compare;
                
        when compare =>
            if data_store(index) > data_store(max_index) then
                nextState <= new_max;
            else
                nextState <= req_data;
            end if;
        
        when new_max => 
            nextState <= req_data;
        
        when end_data =>
            nextState <= set_low1;
            
        when set_low1 =>
            nextState <= set_low2;
        
        when set_low2 =>
            nextState <= set_low3;
       
        when set_low3 =>
            nextState <= set_max;
        
        when set_max =>
            nextState <= set_high1;
        
        when set_high1 =>
            nextState <= set_high2;
        
        when set_high2 =>
            nextState <= set_high3;
        
        when set_high3 =>
            nextState <= send_results;
            
        when send_results =>
            nextState <= wait_start;
    end case;
end process;    

--progress to next state    
next_state_seq : process(clk, reset)
begin
    if reset = '1' then
        curState <= wait_start;
    elsif rising_edge(clk) then
        curState <= nextState;
    end if;
end process;        


end asm;
