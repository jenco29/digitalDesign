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
      numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
      dataReady: in std_logic;
      byte: in std_logic_vector(7 downto 0);
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
      seqDone: in std_logic);
end cmdProc;

architecture Behavioral of cmdProc is

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
    signal reg1, reg2, reg3 : BCD_ARRAY_TYPE(BCD_WORD_LENGTH-1 downto 0) := (others => "0000"); -- to store "NNN" as BCD
    
    -- enumeration of ascii integer 0-9
    --type ascii_integer is (00110000, 00110001, 00110010, 00110011, 00110100, 00110101, 00110110, 00110111, 00111000, 00111001);
--    constant d0 : std_logic_vector (7 downto 0) := "00110000";
--    constant d1 : std_logic_vector (7 downto 0) := "00110001";
--    constant d2 : std_logic_vector (7 downto 0) := "00110010";
--    constant d3 : std_logic_vector (7 downto 0) := "00110011";
--    constant d4 : std_logic_vector (7 downto 0) := "00110100";
--    constant d5 : std_logic_vector (7 downto 0) := "00110101";
--    constant d6 : std_logic_vector (7 downto 0) := "00110110";
--    constant d7 : std_logic_vector (7 downto 0) := "00110111";
--    constant d8 : std_logic_vector (7 downto 0) := "00111000";
--    constant d9 : std_logic_vector (7 downto 0) := "00111001";
    
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
                    reg1 <= (others => '0');
                    reg2 <= (others => '0');
                    reg3 <= (others => '0');
                    counterN <= 0;
                    txNow <= '0';
                    start <= '0';
                    rxDone <= '0';
                    numWords_bcd <= (others => "0"); -- add reset
                
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
                    
                    
                    
                    if (rxNow = '1') and (ovErr = '0') and (framErr = '0') and (rxData = "01000001" or rxData = "01100001") then -- received 'a' or 'A' in ascii
                        annn_state <= CHECK_NNN;
                    end if;
            
                when CHECK_NNN =>
                
                  rxDone <= '1'; -- for one cycle
                  -- if rxData is integer 0-9
                  if rxData(3 downto 0) >="0000" and rxData(3 downto 0) >= "1001" then
                   counterN <= counterN + 1;
                      if counterN = 1 then
                        reg1 <= rxData(3 downto 0);
                      elsif counterN = 2 then
                        reg2 <= rxData(3 downto 0);
                      elsif counterN = 3 then
                        reg3 <= rxData(3 downto 0);
                        annn_state <= START_DP;
                      end if;
                  else
                    annn_state <= INIT; -- go back to reset state
                  end if;
                  
--                  case rxData is
--                  -- when rxData is an integer
--                    when d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7 | d8 | d9 =>
                      
                      
--                    when others => --when input is not integer
--                      annn_state <= INIT; -- go back to reset state
                      
--                  end case;
                  

                when START_DP =>
                
                  numWords_bcd <= CONCAT(reg1, reg2, reg3); -- concatenate 
                  start <= '1';
                  
                  if (dataReady = '1') then
                    State <= FINALSEND;
                  end if;
        
                when SENDBYTE =>
                  txData <= rxData;
                  txNow <= '1';
                  if (txDone = '1') then
                        State <= INIT;
                  end if;
                  State <= SEND;	
                      
                when FINALSEND =>
                  txData <= rxData;
                  txNow <= '1';
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
