library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.common_pack.all;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;


entity cmdProc is
    Port (       
      clk:	in std_logic;
      reset: in std_logic;
      
      rxNow: in std_logic; -- valid
      rxData: in std_logic_vector (7 downto 0); -- rx data
      rxDone: out std_logic; --rx done
      
      ovErr: in std_logic;
      framErr:	in std_logic;
      
      txData: out std_logic_vector (7 downto 0);
      txNow: out std_logic;
      txDone: in std_logic;
      
      start: out std_logic;
      numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
      dataReady: in std_logic; -- Data is valid when dataReady is high
      byte: in std_logic_vector(7 downto 0);
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0); --Contains the index of the peak byte in BCD format.
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- need for L command
      seqDone: in std_logic);
end cmdProc;


architecture Behavioral of cmdProc is

function to_bcd(value : integer) return BCD_ARRAY_TYPE is
    variable bcd : BCD_ARRAY_TYPE(0 to 0);
begin
    case value is
        when 0 => bcd(0) := "0000";
        when 1 => bcd(0) := "0001";
        when 2 => bcd(0) := "0010";
        when 3 => bcd(0) := "0011";
        when 4 => bcd(0) := "0100";
        when 5 => bcd(0) := "0101";
        when 6 => bcd(0) := "0110";
        when 7 => bcd(0) := "0111";
        when 8 => bcd(0) := "1000";
        when 9 => bcd(0) := "1001";
        when others => bcd(0) := "0000";
    end case;
    return bcd;
end function;

function to_hex(value : std_logic_vector) return std_logic_vector is
    variable result : std_logic_vector(7 downto 0);
    begin
    if value >= "0000" and value <= "1001" then
        result := "0011" & value;
    elsif value = "1010" then -- 10
        result := "01000001";
    elsif value = "1011" then -- 11
        result := "01000010";
    elsif value = "1100" then -- 12
        result := "01000011";
    elsif value = "1101" then -- 13
        result := "01000100";
    elsif value = "1010" then -- 14
        result := "01000101";
    elsif value = "1011" then -- 15
        result := "01000110";
    else
        result := "00000000";
    
    end if;
    return result;
 end function;

-----------defining different state types for top-level FSM

    type top_state_type is (INIT, PL, ANNN);
    signal top_state : top_state_type := INIT;


    type data_echo_state_type is (INIT, ECHO); -- is this necessary
    signal data_echo_state : data_echo_state_type := INIT;
    
    type pl_state_type is (INIT, PEAK, LIST);
    signal pl_state : pl_state_type := INIT;
    signal next_pl_state : pl_state_type := INIT;
    
    type annn_state_type is (INIT, CHECK_NNN, START_DP, SEND);
    signal annn_state : annn_state_type := INIT;
    signal next_annn_state : ANNN_state_type := INIT;
    
    -- counters and register declarations
    signal counterN, index_counter : integer range 0 to 3 := 0; -- to validate ANNN input
    signal reg1, reg2, reg3, rxSignal : BCD_ARRAY_TYPE(3 downto 0) := (others => "0000"); -- N registers, and rxSignal to convert binary to BCD
    signal peakSent, indexSent, spaceSent : bit := '0';
    type INT_ARRAY is array (integer range<>) of integer;
    signal bcd_sum : INT_ARRAY(2 downto 0);
    signal index_reg : integer range 0 to 999 := 0;
    signal index_binary : std_logic_vector(11 downto 0) := (others => '0');
    signal list_counter : integer range 0 to 14 := 0;
    signal send_space : boolean := false;
    signal list_value : std_logic_vector(55 downto 0);
    
    -- constants of symbols in ASCII binary code
    constant lowerp : std_logic_vector (7 downto 0) := "01110000";
    constant upperp : std_logic_vector (7 downto 0) := "01010000";
    constant lowerl : std_logic_vector (7 downto 0) := "01101100";
    constant upperl : std_logic_vector (7 downto 0) := "01001100";
    constant lowera : std_logic_vector (7 downto 0) := "01100001";
    constant uppera : std_logic_vector (7 downto 0) := "01000001";
    constant space : std_logic_vector (7 downto 0) := "00100000";
    constant foo : unsigned (7 downto 0) := x"39";
    
begin


--------------------------- TOP LEVEL FSM

    top_fsm : process (clk, reset)
    begin
    
        if reset = '1' then
            top_state <= INIT;
            --reset to init
            
        elsif rising_edge(clk) then
            case top_state is
                when INIT =>
                    -- init all counters and registers again as 0
                    reg1 <= (others => "0000");
                    reg2 <= (others => "0000");
                    reg3 <= (others => "0000");
                    counterN <= 0;
                    txNow <= '0';
                    start <= '0';
                    rxDone <= '0';
                    txdata <= (others => '0');
                    rxSignal <= (others => "0000");
                    numWords_bcd <= (others => "0"); -- add reset
                    peakSent <= '0';
                    indexSent <= '0';
                    index_counter <= 0;
                
                    if rxNow = '1' then
                        -- if 'a' or 'A' input
                        if rxData = lowera or rxData = uppera then 
                            top_state <= ANNN;  
                        -- if 'l' or 'L' or 'p' or 'P' input
                        elsif rxData = lowerl or rxData = upperl or 
                        rxData = lowerp or rxData = upperp then
                            top_state <= PL;
                        else
                            top_state <= INIT;
                        end if;
                    end if;
                
                when PL =>
                    if pl_state = INIT then
                        top_state <= INIT;
                    end if;
                when ANNN =>
                    if annn_state = INIT then
                        top_state <= INIT;
                        
                    -- if 'p' or 'l' input during CHECK_NNN state then change top level state to PL
                    elsif (annn_state = CHECK_NNN) and 
                    (rxData = lowerp or rxData = upperp or
                    rxData = lowerl or rxData = upperl) then
                        top_state <= PL;
                    end if;   
                when others =>
                    top_state <= INIT;
            end case;
        
        end if;
    
    end process; --end top-level fsm
    

    -------------------- ANNN sub-FSM process
    annn_process : process (clk)
    begin
    next_annn_state <= annn_state;
        if (rising_edge(clk)) then
            case annn_state is
            
                when INIT => -- initial state
                --txdone <= '0';
                    
                    if (rxNow = '1') and (ovErr = '0') and (framErr = '0') and (rxData = "01000001" or rxData = "01100001") then -- received 'a' or 'A' in ascii
                        next_annn_state <= CHECK_NNN;
                    end if;
            
                when CHECK_NNN => -- check input after 'a/A' is 3 integers between 0-9
                
                  rxDone <= '1'; -- for one cycle
                  -- if rxData is integer 0-9
                  if rxData(3 downto 0) >="0000" and rxData(3 downto 0) >= "1001" then
                   counterN <= counterN + 1;
                   
                   case rxData(3 downto 0) is
                   when "0000" =>
                   
                    rxSignal <= (others=>"0000");
                    
                   when "0001" =>
                   
                    rxSignal <= to_bcd(1);
                    
                   when "0010" =>
                    rxSignal <= to_bcd(2);
                    
                   when "0011" =>
                   
                    rxSignal <= to_bcd(3);
                    
                   when "0100" =>
                    rxSignal <= to_bcd(4);
                    
                   when "0101" =>
                   
                    rxSignal <= to_bcd(5);
                    
                   when "0110" =>
                    rxSignal <= to_bcd(6);
                   
                   when "0111" =>
                   
                    rxSignal <= to_bcd(7);
                    
                   when "1000" =>
                    rxSignal <= to_bcd(8);
                   
                   when "1001" =>
                    rxSignal <= to_bcd(9);
                    
                   when others =>
                    rxSignal <= to_bcd(0); -- edge case
                   
                   end case;
                   
                  else
                    next_annn_state <= INIT; -- go back to reset state
                  end if;

                when START_DP =>
                
                  numWords_bcd <= reg1 & reg2 & reg3; -- concatenate 
                  start <= '1';
                  
                  if (dataReady = '1') then
                    next_annn_state <= SEND;
                  end if;
        
                when SEND =>
                  txData <= byte; -- send byte from data proc to Tx
                  txNow <= '1'; --send
                  if (txDone = '1' and seqDone = '1' and dataReady = '1') then --if
                        next_annn_state <= INIT;
                  end if;
                  next_annn_state <= SEND;	
               
            end case;
        end if;
        
        annn_state <= next_annn_state;
    end process;
    
    ------------------------------- REG process to update registers
    
    reg_process : process (clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                reg1 <= (others => "0000");
                reg2 <= (others => "0000");
                reg3 <= (others => "0000");
            elsif annn_state = CHECK_NNN then
                if counterN = 1 then
                    reg1 <= rxSignal;
                elsif counterN = 2 then
                    reg2 <= rxSignal;
                elsif counterN = 3 then
                    reg3 <= rxSignal;
                end if;
            end if;
        end if;
    end process;
    
---------------------- PL FSM

    pl_process : process (clk, reset)
    begin
    next_pl_state <= pl_state;
    if reset = '1' then
            -- reset data echoing state
            next_pl_state <= INIT;
            
    elsif rising_edge(clk) then
    
    case pl_state is
    
        when INIT =>
            if (ovErr = '0') and (framErr = '0') and (rxNow = '1') and 
            ((rxData = lowerp) or (rxData = upperp)) then -- command is PEAK
                next_pl_state <= PEAK;
            
            
             elsif (ovErr = '0') and (framErr = '0') and (rxNow = '1') and 
            ((rxData = lowerl) or (rxData = upperl)) then -- command is LIST
                list_value <= dataResults(0) & dataResults(1) & dataResults(2) & dataResults(3) & dataResults(4) & dataResults(5) & dataResults(6) & dataResults(7);
                next_pl_state <= LIST;
            end if;
            
        
        when PEAK =>
            
            if peakSent = '0' and indexSent = '0' and spaceSent = '0' then
            --send peak value from dataResults
                txData <= dataResults(4); -- ???
                peakSent <= '1';
            
            elsif peakSent = '1' and indexSent = '0' and spaceSent = '0' then
            --send space
                txData <= space;
                spaceSent <= '1';
            
            elsif peakSent = '1' and indexSent = '0' and spaceSent = '1' then
            --send maxIndex
                if index_counter < 4 then
                    txData <= maxIndex(0);
                    txNow <= '1';
                    index_counter <= index_counter + 1;
                else
                    next_pl_state <= INIT;
                
               end if;
                
            else
                next_pl_state <= INIT;
            
            end if;
            
        when LIST =>
            if list_counter < 14 and send_space = false then
                
                --send dataResults(list_counter)
                txData <= list_value(list_counter*4 to list_counter*4 + 3);
                txNow <= '1';
                if list_counter mod 2 = 1 then
                send_space <= true;
                
                end if;
                
                list_counter <= list_counter + 1;
            
            elsif send_space = true then
                
                txData <= space;
                txNow <= '1';
            
                send_space <= false;
            
            
            else
            
               next_pl_state <= pl_state;
               
            end if;
        
    
    end case;
        
    
    end if;
    
    pl_state <= next_pl_state;

    end process;

---------------------- DATA ECHOING FSM [runs concurrently
    data_echoing : process (clk, reset)
    begin
        if reset = '1' then
            -- reset data echoing state
            data_echo_state <= INIT;
        
        elsif rising_edge(clk) then
        
        case data_echo_state is
        
            when INIT =>
                rxDone <= '0';
                -- reset registers etc
                
                if rxNow = '1' then
                    data_echo_state <= ECHO;
                end if;
            when ECHO =>
                txData <= rxData;
                txNow <= '1';
                
                if txDone = '1' then
                    data_echo_state <= INIT;
                end if;
                
        end case;
        end if;
    end process;
                
            


end Behavioral;
