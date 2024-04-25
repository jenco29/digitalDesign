
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
type state_type is (init, set_num, wait_start, check_index, req_data, new_data, read_data, compare, new_max, set_digit1, set_digit2, set_digit3, send_results, seq_done);
signal curState, nextState: state_type;

-- index: position of data point currently being compared
-- max_index: position of largest currently known data point
-- num_words_reg: stores desired number of words from input
signal max_index, num_words_reg: integer range 0 to SEQ_LENGTH -1;
signal index: integer range 0 to SEQ_LENGTH +2;
-- temp: stores the intermediate values of max_index when creating the bcd array
signal temp : integer;
-- en_send : allows the dataResults to be stored when high
signal en_send : boolean;
-- ctrl_out_reg: stores current value of output to data generator, used for toggling
-- ctrlIn_delayed, ctrlIn_detected: values used in logic for finding change in signal from data generator
signal ctrl_out_reg, ctrlIn_delayed, ctrlIn_detected, start_reg : std_logic;
-- max_ind_bcd: A register locally storing the result of converting the index of the peak to a BCD value, which is then outputted
signal max_ind_bcd : BCD_ARRAY_TYPE(0 to 2);
-- data_store: register that stores full sequence of data points from data generator
signal data_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH + 5);

begin

--storing start value inputted on the clock edge and setting ctrlIn_detected to signal change in input signal
reg_ctrl : process(clk)
begin
    if rising_edge(clk) then
        ctrlIn_delayed <= ctrlIn;
        start_reg <= start;
    end if;
end process;

--goes high when there is a change in the ctrlIn signal
ctrlIn_detected <= ctrlIn xor ctrlIn_delayed;

-- process to store each incoming byte on the clock when data is to be read. also stores buffers of zero in first and last three bytes
store_data : process(clk)
begin
    if rising_edge(clk) then
        if curState = set_num then
            byte <= data;
            data_store(0) <= "00000000";
            data_store(1) <= "00000000";
            data_store(2) <= "00000000";
            data_store(3) <= data;
            data_store(SEQ_LENGTH + 3) <= "00000000";
            data_store(SEQ_LENGTH + 4) <= "00000000";
            data_store(SEQ_LENGTH + 5) <= "00000000";
        elsif curState = read_data then
            data_store(index) <= data;
            byte <= data;
        end if;
    end if;
end process;

-- process that converts the index of the peak to a BCD value
int_to_bcd : process(clk) 
begin
    if rising_edge(clk) then
        if curState = set_digit1 then -- set the unit digit of the BCD value
            max_ind_bcd(0) <= std_logic_vector(to_unsigned((max_index - 3) mod 10, BCD_WORD_LENGTH));
            temp <= ((max_index -3) - (max_index - 3) mod 10) / 10; 
        elsif curState = set_digit2 then -- set the tens digit of the BCD value
            max_ind_bcd(1) <= std_logic_vector(to_unsigned(temp mod 10, BCD_WORD_LENGTH));
            temp <= (temp - temp mod 10) / 10;
        elsif curState = set_digit3 then -- set the hundreds digit of the BCD value
            max_ind_bcd(2) <= std_logic_vector(to_unsigned(temp mod 10, BCD_WORD_LENGTH));
        end if;
    end if;
end process;

-- process to read in from the command processor the number of data points required, and convert it from BCD to an integer
set_num_words : process(numWords_bcd)
begin
    num_words_reg <= to_integer(signed(numwords_bcd(0))) + (to_integer(signed(numwords_bcd(1))) * 10) + (to_integer(signed(numwords_bcd(2))) * 100);
end process;

-- on the enable send signal, dataResults and maxIndex are set to their final values
write_results : process(clk)
begin
    if rising_edge(clk) then
        if en_send = true then
            dataresults(6) <= data_store(max_index -3);
            dataresults(5) <= data_store(max_index -2);
            dataresults(4) <= data_store(max_index -1);
            dataresults(3) <= data_store(max_index );
            dataresults(2) <= data_store(max_index +1);
            dataresults(1) <= data_store(max_index +2);
            dataresults(0) <= data_store(max_index +3);
            maxIndex <= max_ind_bcd;
        end if;
    end if;
end process;

--reset to 3 due to the buffer on init, max_index is set to index in the state new_max.
set_max_index : process(clk)
begin
    if rising_edge(clk) then
        if curState = init then
            max_index <= 3;
        elsif curState = new_max then
            max_index <= index;
        end if;
    end if;
end process;  

--increments the index everytime data is requested, starting from 3 due to the buffer.
count : process(clk)
begin
    if rising_edge(clk) then
        if curState = init then
            index <= 3;
        elsif curState = req_data then
            index <= index + 1;
        end if;
    end if;
end process;

--resets ctrlOut in initial state, then toggles it based on the registered value in req_data state.
swap_ctrlOut : process(curState)
begin
    if curState = init then
        ctrlOut <= '0';
        ctrl_out_reg <= '0';
    elsif curState = req_data then
        if ctrl_out_reg = '1' then
            ctrlOut <= '0';
            ctrl_out_reg <= '0';
        else
            ctrlOut <= '1';
            ctrl_out_reg <= '1';
        end if;
    end if;
end process;


--processes on asm states
state_logic : process(curState)
begin
    dataReady <= '0';
    seqDone <= '0';
    en_send <= false;
    
    case curState is
           
        when read_data => 
            dataReady <= '1';
                    
        when send_results =>
            en_send <= true;
        
        when seq_done =>
            seqDone <= '1';
               
        when others =>
            -- do nothing
    end case;
end process;

--changing asm states
next_state_comb : process(curState, clk, start, ctrlIn_detected, index, data_store, num_words_reg, max_index)
begin
    case curState is 
        when init =>
            nextState <= set_num;
            
        when set_num =>
            nextState <= wait_start;
            
        when wait_start =>
            if start_reg = '1' then
                nextState <= req_data;
            else
                nextState <= wait_start;
            end if;           
                     
        when req_data =>
            nextState <= new_data;
            
        when new_data =>
            if ctrlIn_detected = '1' then
                nextState <= read_data;
            else 
                nextState <= new_data;
            end if;
            
        when read_data =>
            nextState <= check_index;
            
        when check_index =>
            if index - 3 < num_words_reg then
                nextState <= compare;
            else
                nextState <= set_digit1;
            end if;
       
        when compare =>
            if signed(data_store(index)) > signed(data_store(max_index)) then
                nextState <= new_max;
            else
                nextState <= wait_start;
            end if;
        
        when new_max => 
            nextState <= wait_start;
            
        when set_digit1 =>
            nextState <= set_digit2;
            
        when set_digit2 =>
            nextState <= set_digit3;
        
        when set_digit3 =>
            nextState <= send_results;
            
        when send_results =>
            nextState <= seq_done;
            
        when seq_done =>
            nextState <= init;
            
    end case;
end process;         

--progress to next state  
next_state_seq : process(clk, reset)
begin
    if reset = '1' then
        curState <= init;
    elsif rising_edge(clk) then
        curState <= nextState;
    end if;
end process;

end asm;
