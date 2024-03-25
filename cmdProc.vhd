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
      rxNow: in std_logic;
      rxData: in std_logic_vector (7 downto 0);
      txData: out std_logic_vector (7 downto 0);
      rxDone: out std_logic;
      ovErr: in std_logic;
      framErr:	in std_logic;
      txNow: out std_logic;
      txDone: in std_logic;
      start: out std_logic;
      numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
      dataReady: in std_logic;
      byte: in std_logic_vector(7 downto 0));
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
end cmdProc;

architecture Behavioral of cmdProc is

-----------defining different state types for top-level FSM

    type top_state_type is (INIT, PL, ANNN);
    signal top_state : top_state_type := INIT;


    --type data_echo_state_type is (INIT, ECHO);
    --signal data_echo_state : data_echo_state_type := INIT;
    
    type pl_state_type is (INIT, START_PROC, SEND_TX);
    signal pl_state : pl_state_type := INIT;
    
    type ANNN_state_type is (INIT, CHECK_NNN, START_DP, SEND);
    signal annn_state : ANNN_state_type := INIT;
    
    -- counters and register declarations
    signal counterN : integer range 0 to 3 := 0; -- to validate ANNN input
    signal reg1, reg2, reg3 : std_logic_vector(3 downto 0); -- to store "NNN" as BCD

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
                    if rxNow = '1' then
                        -- if 'a' or 'A' input
                        if rxData = "01000001" or rxData = "01100001" then 
                            top_state <= ANNN;  
                        -- if 'l' or 'L' or 'p' or 'P' input
                        elsif rxData = "01001100" or rxData = "01101000" or rxData = "01010000" or rxData = "01110000" then
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
                    end if;      
            end case;
        
        end if;
    
    end process; --end top-level fsm

    -------------------- ANNN sub-FSM process
    ANNN_process : process (CLK)
    begin
        if (rising_edge(CLK)) then
            case annn_state is
            
                when INIT => -- initial state
                --txdone <= '0';
                    txNow <= '0';
                    start <= '0';
                    done <= '0';
                    numWords_bcd <= '0'; -- add reset
                    
                    if (valid = '1') and (oe = '0') and (fe = '0') and (rxData = '01000001' or '01100001') then -- received 'a' or 'A' in ascii
                    end if;
            
                when CHECKNNN =>
                
                  --done <= '1'
                
                  if rxData = integer --0 to 9		  
                      counterN <= counterN + 1;
                  end if;
                  
                  if counterN = 1 then
                      reg1 = N;
                  else if counterN = 2 then
                      reg2 = N;
                  else if counterN = 3 then
                      reg3 = N;
                  end if;
                  
                when START_DP =>
                  numWords_bcd <= reg1 & reg2 & reg3 -- concatenate 
                  start <= '1'
                  
                  if (dataReady = '1') then
                    State <= FINALSEND;
                  end if;
        
                when SENDBYTE =>
                  txData <= rxData
                  txNow <= '1'
                  if (txDone = '1') then
                        State <= INIT;
                  end if;
                  State <= SEND;	
                      
                when FINALSEND =>
                  txData <= rxData
                  txNow <= '1'
                  if (txDone = '1') then
                        State <= INIT;
              end if;
              State <= SEND;
            when others=> //should never be reached
                State <= INIT;
            end case;
        end if;
    if (reset = '1') then
        currentState <= INIT;
    end if;
    end process;
    
---------------------- PL FSM



---------------------- DATA ECHOING FSM

end Behavioral;
