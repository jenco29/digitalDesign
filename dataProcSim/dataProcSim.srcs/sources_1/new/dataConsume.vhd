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
type state_type is (wait_start, set_num, check_index, prep_data, req_data, new_data, read_data, compare, new_max, end_data, set_results, set_digits, set_max, send_results);
signal curState, nextState: state_type;

-- index: position of data point currently being compared
-- max_index: position of largest currently known data point
-- num_words_reg: stores desired number of words from input
signal index, index_reg, max_index, num_words_reg: integer range 0 to SEQ_LENGTH -1;
-- data_reg: register to synchronously store current data point
signal data_reg: std_logic_vector(7 downto 0);
-- ctrl_out_reg: stores current value of output to data generator, used for toggling
-- ctrlIn_delayed, ctrlIn_detected: values used in logic for finding change in signal from data generator
-- read_done: 
-- dataReady_reg: 
signal ctrl_out_reg, ctrlIn_delayed, ctrlIn_detected, read_done, dataReady_reg, start_reg : std_logic;
signal data_results_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
signal max_ind_reg : BCD_ARRAY_TYPE(0 to 2);
signal clk_rise_toggle : std_logic;
type INT_ARRAY is array (integer range<>) of integer;
-- data_store: register that stores full sequence of data points from data generator
signal data_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH -1);
signal bcd_sum, digits, mod_list: INT_ARRAY(2 downto 0);

begin

toggle_clk: process(clk)
begin
    if rising_edge(clk) then
        clk_rise_toggle <= not clk_rise_toggle;
    end if;
end process;

--storing data value inputted on the clock edge
reg_data : process(clk)
begin
    if rising_edge(clk) then
        data_reg <= data;
        start_reg <= start;
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
state_logic : process(curState, clk)
begin
    case curState is
        when wait_start =>
            for i in 0 to 2 loop
                digits(i) <= 0;
                mod_list(i) <= 0;
            end loop;
            num_words_reg <= 0;
            index <= 0;
            ctrlOut <= '0';
            ctrl_out_reg <= '0'; 
            max_index <= 0;
        
        when set_num =>
            data_store(0) <= data_reg;
            byte <= data_reg;
            bcd_sum(0) <= to_integer(signed(numwords_bcd(0)));
            bcd_sum(1) <= to_integer(signed(numwords_bcd(1))) * 10;
            bcd_sum(2) <= to_integer(signed(numwords_bcd(2))) * 100;
            num_words_reg <= bcd_sum(0) + bcd_sum(1) + bcd_sum(2);
           
        when prep_data =>
            ctrl_out_reg <= not ctrl_out_reg;
            index_reg <= index + 1;
           
        when req_data =>
            ctrlOut <= ctrl_out_reg;
            index <= index_reg;
        
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
            
        when set_results =>
            data_results_reg(0) <= data_store(max_index -3);
            data_results_reg(1) <= data_store(max_index -2);
            data_results_reg(2) <= data_store(max_index -1);
            data_results_reg(3) <= data_store(max_index );
            data_results_reg(4) <= data_store(max_index +1);
            data_results_reg(5) <= data_store(max_index +2);
            data_results_reg(6) <= data_store(max_index +3);
            seqDone <= '1';
            
            mod_list(2) <= max_index;
            mod_list(1) <= (mod_list(2) - mod_list(2) mod 10)/10;
            mod_list(0) <= (mod_list(1) - mod_list(1) mod 10)/10;
            
        when set_digits =>
            digits(2) <= mod_list(2) mod 10;
            digits(1) <= mod_list(1) mod 10;
            digits(0) <= mod_list(0) mod 10;
        
        when set_max =>
            max_ind_reg(2) <= std_logic_vector(to_unsigned(digits(2), BCD_WORD_LENGTH));
            max_ind_reg(1) <= std_logic_vector(to_unsigned(digits(1), BCD_WORD_LENGTH));
            max_ind_reg(0) <= std_logic_vector(to_unsigned(digits(0), BCD_WORD_LENGTH));
            
        when send_results =>
            dataResults <= data_results_reg;
            maxIndex <= max_ind_reg;
               
        when others =>
            -- do nothing
    end case;
end process;

--changing asm states
next_state_comb : process(curState, clk)
begin
    case curState is 
        when wait_start =>
            if start_reg = '1' then
                nextState <= set_num;
            else
                nextState <= wait_start;
            end if;
            
        when set_num =>
            nextState <= prep_data;
            
        when prep_data =>
            nextState <= req_data;
            
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
            if signed(data_store(index)) > signed(data_store(max_index)) then
                nextState <= new_max;
            else
                nextState <= prep_data;
            end if;
        
        when new_max => 
            nextState <= prep_data;
        
        when end_data =>
            nextState <= set_results;

        when set_results =>
            nextState <= set_digits;
            
        when set_digits =>
            nextState <= set_max;
            
        when set_max =>
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
