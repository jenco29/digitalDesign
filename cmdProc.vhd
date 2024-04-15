library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.common_pack.all;

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
      numWords_bcd: out BCD_ARRAY_TYPE(11 downto 0);
      dataReady: in std_logic;
      byte: in std_logic_vector(7 downto 0);
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
      seqDone: in std_logic);
end cmdProc;


architecture Behavioral of cmdProc is
function to_bcd(value : integer) return BCD_ARRAY_TYPE is
    variable bcd : BCD_ARRAY_TYPE;
begin
    case value is
        when 0 => bcd := (others => '0');
        when 1 => bcd := "0001";
        when 2 => bcd := "0010";
        when 3 => bcd := "0011";
        when 4 => bcd := "0100";
        when 5 => bcd := "0101";
        when 6 => bcd := "0110";
        when 7 => bcd := "0111";
        when 8 => bcd := "1000";
        when 9 => bcd := "1001";
        when others => bcd := (others => '0');
    end case;
    return bcd;
end function;

-----------defining different state types for top-level FSM

    type top_state_type is (INIT, PL, ANNN);
    signal top_state : top_state_type := INIT;


    type data_echo_state_type is (INIT, ECHO); -- is this necessary
    signal data_echo_state : data_echo_state_type := INIT;
    
    type pl_state_type is (INIT, START_PROC, SEND_TX);
    signal pl_state : pl_state_type := INIT;
    
    type ANNN_state_type is (INIT, CHECK_NNN, START_DP, SEND);
    signal annn_state : ANNN_state_type := INIT;
    
    -- counters and register declarations
    signal counterN : integer range 0 to 3 := 0; -- to validate ANNN input
    signal reg1, reg2, reg3, rxSignal : BCD_ARRAY_TYPE(3 downto 0) := (others => "0000"); -- to store "NNN" as BCD
    
    constant lowerp : std_logic_vector (7 downto 0) := "01110000";
    constant upperp : std_logic_vector (7 downto 0) := "01010000";
    constant lowerl : std_logic_vector (7 downto 0) := "01101100";
    constant upperl : std_logic_vector (7 downto 0) := "01001100";
    constant lowera : std_logic_vector (7 downto 0) := "01100001";
    constant uppera : std_logic_vector (7 downto 0) := "01000001";
    
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
                    numWords_bcd <= (others => "0"); -- add reset
                
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
            end case;
        
        end if;
    
    end process; --end top-level fsm
    

    -------------------- ANNN sub-FSM process
    annn_process : process (CLK)
    begin
        if (rising_edge(CLK)) then
            case annn_state is
            
                when INIT => -- initial state
                --txdone <= '0';
                    
                    
                    
                    if (rxNow = '1') and (ovErr = '0') and (framErr = '0') and (rxData = "01000001" or rxData = "01100001") then -- received 'a' or 'A' in ascii
                        annn_state <= CHECK_NNN;
                    end if;
            
                when CHECK_NNN =>
                
                  rxDone <= '1'; -- for one cycle
                  -- if rxData is integer 0-9
                  if rxData(3 downto 0) >="0000" and rxData(3 downto 0) >= "1001" then
                   counterN <= counterN + 1;
                   
                   case rxData(3 downto 0) is
                   WHEN "0000" =>
                   
                    rxSignal <= (others=>"0000");
                    
                   WHEN "0001" =>
                   
                    rxSignal <= to_bcd(1);
                    
                   WHEN "0010" =>
                    rxSignal <= to_bcd(2);
                    
                   WHEN "0011" =>
                   
                    rxSignal <= to_bcd(3);
                    
                   WHEN "0100" =>
                    rxSignal <= to_bcd(4);
                    
                   WHEN "0101" =>
                   
                    rxSignal <= to_bcd(5);
                    
                   WHEN "0110" =>
                    rxSignal <= to_bcd(6);
                   
                   WHEN "0111" =>
                   
                    rxSignal <= to_bcd(7);
                    
                   WHEN "1000" =>
                    rxSignal <= to_bcd(8);
                   
                   WHEN "1001" =>
                    rxSignal <= to_bcd(9);
                   
                   end case;
                   
                 
                      if counterN = 1 then
                        reg1 <= rxSignal;
                      elsif counterN = 2 then
                        reg2 <= rxSignal;
                      elsif counterN = 3 then
                        reg3 <= rxSignal;
                        annn_state <= START_DP;
                      else
                        annn_state <= INIT;
                      end if;
                      
                  else
                    annn_state <= INIT; -- go back to reset state
                  end if;

                when START_DP =>
                
                  numWords_bcd <= reg1 & reg2 & reg3; -- concatenate 
                  start <= '1';
                  
                  if (dataReady = '1') then
                    annn_state <= SEND;
                  end if;
        
                when SEND =>
                  txData <= rxData;
                  txNow <= '1';
                  if (txDone = '1') then
                        annn_state <= INIT;
                  end if;
                  annn_state <= SEND;	
               
            end case;
        end if;
    end process;
    
---------------------- PL FSM

    pl_process : process (clk, reset)
    begin
    if reset = '1' then
            -- reset data echoing state
            pl_state <= INIT;
    elsif rising_edge(clk) then
    
    case pl_state is
    
        
        when INIT =>
            if (ovErr = '0') and (framErr = '0') and (rxNow = '1') and 
            ((rxData = lowerp) or (rxData = upperp) or (rxData = lowerl) or (rxData = upperl)) then
                pl_state <= START_PROC;
            end if;
        
        when START_PROC =>
            rxDone <= '1';
            start <= '1';
            if seqDone = '1' then
                pl_state <= SEND_TX;
            end if;
        
        when SEND_TX =>
            txNow <= '1'; -- SEND data
            if txDone = '1' or reset = '1' then
                pl_state <= INIT;
            end if;
    
    
    end case;
        
    
    end if;
    

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
