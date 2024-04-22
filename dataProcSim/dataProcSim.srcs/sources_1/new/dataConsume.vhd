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
type state_type is (wait_start, set_num, check_index, prep_data, req_data, new_data, read_data, compare, new_max, end_data, set_results, set_digit1, set_digit2, set_digit3, send_results);
signal curState, nextState: state_type;

-- index: position of data point currently being compared
-- max_index: position of largest currently known data point
-- num_words_reg: stores desired number of words from input
signal index, max_index, num_words_reg: integer range 0 to SEQ_LENGTH -1;
-- temp #EXPLAIN
signal temp : integer;
-- data_reg: register to synchronously store current data point
signal data_reg: std_logic_vector(7 downto 0);
-- #EXPLAIN
signal en_num_words, en_set_max, en_store, en_send : boolean;
-- ctrl_out_reg: stores current value of output to data generator, used for toggling
-- ctrlIn_delayed, ctrlIn_detected: values used in logic for finding change in signal from data generator
-- read_done: #EXPLAIN
-- dataReady_reg: #EXPLAIN
signal ctrl_out_reg, ctrlIn_delayed, ctrlIn_detected, read_done, dataReady_reg, start_reg : std_logic;
-- data_results_reg: An array of data values storing the peak and surrounding bytes
signal data_results_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
-- max_ind_bcd: A register locally storing the result of converting the index of the peak to a BCD value, which is then outputted
signal max_ind_bcd : BCD_ARRAY_TYPE(0 to 2);
type INT_ARRAY is array (integer range<>) of integer;
-- data_store: register that stores full sequence of data points from data generator
signal data_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH -1);

begin

-- process to store each incoming byte on the clock when data is to be read
store_data : process(clk)
begin
    if rising_edge(clk) then
        if curState = set_num then
            byte <= data_reg;
            data_store(0) <= data_reg;
        elsif curState = read_data then
            data_store(index) <= data_reg;
            byte <= data_reg;
        end if;
    end if;
end process;

-- process that converts the index of the peak to a BCD value
int_to_bcd : process(clk)
begin
    if rising_edge(clk) then
        if curState = set_digit1 then -- set the unit digit of the BCD value
            max_ind_bcd(0) <= std_logic_vector(to_unsigned(max_index mod 10, BCD_WORD_LENGTH));
            temp <= (max_index - max_index mod 10) / 10; 
        elsif curState = set_digit2 then -- set the tens digit of the BCD value
            max_ind_bcd(1) <= std_logic_vector(to_unsigned(temp mod 10, BCD_WORD_LENGTH));
            temp <= (temp - temp mod 10) / 10;
        elsif curState = set_digit3 then -- set the hundreds digit of the BCD value
            max_ind_bcd(2) <= std_logic_vector(to_unsigned(temp mod 10, BCD_WORD_LENGTH));
        end if;
    end if;
end process;

-- process to read in fromt he command processor the number of data points required, and convert it from BCD to an integer
set_num_words : process(clk)
begin
    if rising_edge(clk) then
        if en_num_words = true then
            num_words_reg <= to_integer(signed(numwords_bcd(0))) + (to_integer(signed(numwords_bcd(1))) * 10) + (to_integer(signed(numwords_bcd(2))) * 100);
        end if;
    end if;
end process;

data_results : process(clk)
begin
    if rising_edge(clk) then
        if en_store = true then
            data_results_reg(0) <= data_store(max_index -3);
            data_results_reg(1) <= data_store(max_index -2);
            data_results_reg(2) <= data_store(max_index -1);
            data_results_reg(3) <= data_store(max_index );
            data_results_reg(4) <= data_store(max_index +1);
            data_results_reg(5) <= data_store(max_index +2);
            data_results_reg(6) <= data_store(max_index +3);
        end if;
    end if;
end process;

write_results : process(clk)
begin
    if rising_edge(clk) then
        if en_send = true then
            dataResults <= data_results_reg;
            maxIndex <= max_ind_bcd;
        end if;
    end if;
end process;

set_max_index : process(clk)
begin
    if rising_edge(clk) then
        if curState = wait_start then
            max_index <= 0;
        elsif curState = new_max then
            max_index <= index;
        end if;
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

count : process(clk)
begin
    if rising_edge(clk) then
        if curState = wait_start then
            index <= 0;
        elsif curState = prep_data then
            index <= index + 1;
        end if;
    end if;
end process;

ctrlOut_buffer : process(clk)
begin
    if rising_edge(clk) then
        if curState = wait_start then
            ctrl_out_reg <= '0';
        elsif curState = prep_data then
            ctrl_out_reg <= not ctrl_out_reg;
        end if;
    end if;
end process;

swap_ctrlOut : process(clk)
begin
    if rising_edge(clk) then
        if curState = wait_start then
            ctrlOut <= '0';
        elsif curState = req_data then
            ctrlOut <= ctrl_out_reg;
        end if;
    end if;
end process;


ctrlIn_detected <= ctrlIn xor ctrlIn_delayed;

--processes on asm states
state_logic : process(curState, clk)
begin
    dataReady <= '0';
    seqDone <= '0';
    read_done <= '0';
    en_store <= false;
    en_send <= false;
    en_num_words <= false;
    en_set_max <= false;
    
    case curState is
        
        when set_num =>
            en_num_words <= true;
           
        when read_data => 
            dataReady <= '1';
        
        when end_data =>
            read_done <= '1';
            
        when set_results =>
            seqDone <= '1';
            en_store <= true;
                        
        when send_results =>
            en_send <= true;
               
        when others =>
            -- do nothing
    end case;
end process;

--changing asm states
next_state_comb : process(curState, clk, start_reg, ctrlIn_detected, index, data_store, num_words_reg, max_index)
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
            nextState <= set_digit1;
            
        when set_digit1 =>
            nextState <= set_digit2;
            
        when set_digit2 =>
            nextState <= set_digit3;
        
        when set_digit3 =>
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
