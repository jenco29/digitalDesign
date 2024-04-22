library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.common_pack.all;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

entity cmdProc is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           rxnow : in STD_LOGIC; --valid
           rxData : in std_logic_vector (7 downto 0);
           txData : out std_logic_vector (7 downto 0);
           rxdone : out STD_LOGIC;
           ovErr : in STD_LOGIC;
           framErr : in STD_LOGIC;
           txnow : out STD_LOGIC;
           txdone : in STD_LOGIC;
           start : out STD_LOGIC;
           numwords_bcd : out BCD_ARRAY_TYPE(2 downto 0);
           dataReady : in STD_LOGIC;
           byte : in std_logic_vector (7 downto 0);
           maxIndex : in BCD_ARRAY_TYPE(2 downto 0);
           dataResults : in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
           seqDone : in STD_LOGIC);
end cmdProc;

architecture Behavioral of cmdProc is

function to_ascii(value : std_logic_vector) return std_logic_vector is
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

-- State declaration for main FSM
  TYPE state_type IS (INIT, DATA_ECHO, INIT_BYTE, A, AN, ANN, ANNN, ANNN_BYTE_IN, ANNN_BYTE_OUT1, ANNN_BYTE_OUT2, ANNN_DONE ,P, P_BYTE1, P_BYTE2, P_SPACE, P_INDEX1,P_INDEX2,P_INDEX3, LIST_INIT, LIST_PRINT1, LIST_PRINT2);  -- List your states here 	
  SIGNAL topCurState, topNextState: state_type;
    
    signal data_reg, byte_reg: std_logic_vector(7 downto 0);   -- data_reg: register to synchronously store byte from rx

    signal to_be_sent: std_logic_vector(7 downto 0); --to store the next byte to be sent to tx in hex
    signal ANNN_reg : BCD_ARRAY_TYPE(3 downto 0); -- N registers
    
    signal nibble1, nibble2: std_logic_vector(3 downto 0);
    signal peakStore, listStore: std_logic_vector(7 downto 0);
    
    signal ListCount, ANNN_byteCount,NNN : integer :=0;

    signal enSend, enSent, peakStored, listStored, NNNStored, start_data_echo : boolean := false;

    signal rxnow_reg, txdone_reg, dataReady_reg, seqDone_reg  : std_logic;
    signal maxIndex_reg : BCD_ARRAY_TYPE(3 downto 0);
    signal dataResults_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- N registers

        -- constants of symbols in ASCII binary code
    constant lowerp : std_logic_vector (7 downto 0) := "01110000";
    constant upperp : std_logic_vector (7 downto 0) := "01010000";
    constant lowerl : std_logic_vector (7 downto 0) := "01101100";
    constant upperl : std_logic_vector (7 downto 0) := "01001100";
    constant lowera : std_logic_vector (7 downto 0) := "01100001";
    constant uppera : std_logic_vector (7 downto 0) := "01000001";
    constant space : std_logic_vector (7 downto 0) := "00100000";
    constant num_ascii : std_logic_vector (3 downto 0) := "0011";



BEGIN

---------------store byte from rx--------------------------
reg_txdone : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            txdone_reg <= txdone;
    end if;
end process;  

reg_dataReady : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            dataReady_reg <= dataReady;
    end if;
end process; 

reg_seqDone : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            seqDone_reg <= seqDone;
    end if;
end process; 

reg_rxdata : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
        if rxnow_reg = '1' then
            data_reg <= rxdata;
        end if;
    end if;
end process;  

reg_byte : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
        if dataReady = '1' then
            byte_reg <= byte;
        end if;
    end if;
end process; 

reg_rxnow : process(clk)
begin
    if rising_edge(clk) then
        rxnow_reg <= rxnow;
    end if;
end process;

  
  
  combi_topNextState: PROCESS(topCurState, clk)
  BEGIN
    CASE topCurState IS
      WHEN INIT =>
        IF rxnow_reg = '1' THEN 
                  topNextState <= INIT_BYTE;
        ELSE
                  topNextState <= INIT;

        END IF;
        
         WHEN INIT_BYTE =>
        IF data_reg = lowera or data_reg = uppera THEN 
          topNextState <= A;
        ELSIF data_reg = lowerp or data_reg = upperp THEN 
          topNextState <= P;
        ELSIF data_reg = lowerl or data_reg = upperl THEN 
          topNextState <= LIST_INIT;
        ELSE
          topNextState <= INIT;
        END IF;

        
        
      WHEN A =>
        IF (data_reg(7 downto 0) = num_ascii) and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= AN;
        ELSE
            topNextState <= INIT;
        END IF;
             
              WHEN AN =>
        IF (data_reg(7 downto 0) = num_ascii) and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= ANN;
        ELSE
            topNextState <= INIT;
        END IF;
               
              WHEN ANN =>
        IF (data_reg(7 downto 0) = num_ascii) and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= ANNN;
        ELSE
            topNextState <= INIT;
        END IF;
               
              WHEN ANNN =>
        IF NNNStored = true THEN 
            topNextState <= ANNN_BYTE_IN;
        ELSE
            topNextState <= ANNN;
        END IF;
               
        WHEN ANNN_BYTE_IN =>
        IF dataReady = '1' THEN 
            topNextState <= ANNN_BYTE_OUT1;
        ELSE
           topNextState <= ANNN_BYTE_IN;
        END IF;
               
        WHEN ANNN_BYTE_OUT1 =>
        IF enSent = true THEN 
            topNextState <= ANNN_BYTE_OUT2;
        ELSE
           topNextState <= ANNN_BYTE_OUT1;
        END IF;
        
        
        WHEN ANNN_BYTE_OUT2 =>
        IF enSent = true THEN 
        
            IF ANNN_byteCount < NNN-1 THEN
                topNextState <= ANNN_BYTE_OUT1;               
            ELSE
                topNextState <= ANNN_DONE;
            END IF;       
             
        ELSE
             topNextState <= ANNN_BYTE_OUT2;
        END IF;
        
        WHEN ANNN_DONE =>
            IF seqDone='1' THEN
            topNextState <= INIT;
            ELSE
            topNextState <= ANNN_DONE;
        END IF;
             
             
         WHEN P =>
            IF peakStored = true THEN 
                topNextState <= P_BYTE1;
            ELSE
               topNextState <= P;
             END IF;
             
          WHEN P_BYTE1 =>
            IF enSent = true THEN 
                topNextState <= P_BYTE2;
            ELSE
               topNextState <= P_BYTE1;                
            END IF;
             
         WHEN P_BYTE2 =>
            IF enSent = true THEN 
                topNextState <= P_SPACE;
             ELSE
               topNextState <= P_BYTE2;               
             END IF;
             
         WHEN P_SPACE =>
            IF enSent = true THEN 
                topNextState <= INIT;
            ELSE
               topNextState <= P_SPACE;
             END IF;                     
             
        WHEN P_INDEX1 =>
            IF enSent = true THEN 
                topNextState <= P_INDEX2;
            ELSE
               topNextState <= P_INDEX2;
             END IF; 
             
         WHEN P_INDEX2 =>
            IF enSent = true THEN 
                topNextState <= P_INDEX3;
            ELSE
               topNextState <= P_INDEX2;
             END IF;        
                   
       WHEN P_INDEX3 =>
            IF enSent = true THEN 
                topNextState <= INIT;
            ELSE
               topNextState <= P_INDEX3;
             END IF; 
        
       WHEN LIST_INIT =>
        IF listCount = 7 THEN 
          topNextState <= INIT;
        ELSE
          topNextState <= LIST_PRINT1;      
        END IF;
                     
      WHEN LIST_PRINT1 =>
            IF enSent = true THEN 
                topNextState <= LIST_PRINT2;
        ELSE
                topNextState <= LIST_PRINT1;    
             END IF;
        
       WHEN LIST_PRINT2 =>
            IF enSent = true THEN 
                topNextState <= LIST_INIT;
           ELSE
                topNextState <= LIST_PRINT2;
             END IF;
             
             WHEN others =>

        
    END CASE;
  END PROCESS; -- combi_nextState
  -----------------------------------------------------
  
  --processes on fsm states
state_logic : process(topCurState, clk)
begin
    case topCurState is
        
        when INIT =>
            rxdone <= '0';
            start <= '0';
            txNow <='0';
            numWords_bcd(0) <= "0000";
            numWords_bcd(1) <= "0000";
            numWords_bcd(2) <= "0000";

        when A => 
          ANNN_reg(0) <= data_reg(3 downto 0);
          
        when AN => 
          ANNN_reg(1) <= data_reg(3 downto 0);
                        
          
        when ANN => 
          ANNN_reg(2) <= data_reg(3 downto 0);
    
                  
        when ANNN => --start proc
          NNN <= ( (TO_INTEGER(UNSIGNED(ANNN_reg(0)))) + (TO_INTEGER(UNSIGNED(ANNN_reg(1)))*10) + (TO_INTEGER(UNSIGNED(ANNN_reg(2)))*100));
          start <= '1'; 
          numwords_bcd(0) <= ANNN_reg(0);
          numwords_bcd(1) <= ANNN_reg(1);
          numwords_bcd(2) <= ANNN_reg(2);
          NNNStored <= true;

        
        when ANNN_BYTE_IN   =>           
          nibble1 <= byte(3 downto 0); 
          nibble2 <= byte(7 downto 4); 
          
          --int1 <= TO_INTEGER (UNSIGNED(nibble1));
          --int2 <= TO_INTEGER (UNSIGNED(nibble1));


        when ANNN_BYTE_OUT1  =>
          to_be_sent <= to_ascii(nibble1); 
          enSend <= true;

          
        when ANNN_BYTE_OUT2  =>
          to_be_sent <= to_ascii(nibble2); 
          enSend <= true;
          ANNN_byteCount <= ANNN_byteCount + 1;

          
        when ANNN_DONE => 
          start <= '0';    

        when P =>
          peakStore <= dataResults(3); 
          --int1 <= TO_INTEGER (UNSIGNED(peakStore(3 downto 0)));
          --int2 <= TO_INTEGER (UNSIGNED(peakStore(7 downto 4)));
          peakStored <= true;

          when P_BYTE1 =>

           to_be_sent <= to_ascii(peakStore(3 downto 0));           
            enSend <= true;
            
          when P_BYTE2 =>
           to_be_sent <= to_ascii(peakStore(7 downto 4));                    
            enSend <= true;
            
           when P_SPACE =>         
           to_be_sent <= space;          
            enSend <= true;
            
           when P_INDEX1 =>         
           to_be_sent <= to_ascii(maxIndex(0));          
            enSend <= true;
            
         when P_INDEX2 =>         
            to_be_sent <= to_ascii(maxIndex(1));          
            enSend <= true;
            
           when P_INDEX3 =>         
            to_be_sent <= to_ascii(maxIndex(2));
           enSend <= true;
           

        when LIST_INIT =>
          listStore <= dataResults(listCount); 
          --int1 <= TO_INTEGER (UNSIGNED(listStore(3 downto 0)));
          --int2 <= TO_INTEGER (UNSIGNED(listStore(7 downto 4)));
          listStored <= true;
            
        when LIST_PRINT1 =>          
          to_be_sent <= to_ascii(listStore(3 downto 0));
          enSend <= true;
                        
        when LIST_PRINT2 =>
           to_be_sent <= to_ascii(listStore(7 downto 4));
           enSend <= true;
           listCount <= listCount + 1;                       
                
                                       
        when others =>
            -- do nothing
    end case;
end process;

  ----------------output to tx--------------------------

txData_Out : process(clk)
begin
    if rising_edge(clk) then
        if (topCurState = INIT and rxnow_reg = '1')  then --ADD AND RXNOW='1' BACK IN PLSSSSSSSSSSSSSSSS
               start_data_echo <= true;
        elsif enSend = true then 
            txNow <= '1';
            txData <= to_be_sent;
            if txDone = '1' then   
                  txNow <= '0';        
                  enSent <=true;  
            end if;      
        end if;      
    end if;
end process;

data_echoing : process(clk)
begin
     if  start_data_echo = true then
            txNow <= '1';
            txData <= data_reg;
        if txDone = '1' then           
                rxdone <= '1';
                 txNow <= '0';
         end if;
    end if;      
end process;
  ---------------next state seq------------------------
  
--progress to next state  
next_state_seq : process(clk, reset)
begin
    if reset = '1' then
        topCurState <= INIT;
    elsif rising_edge(clk) then
        topCurState <= topNextState;
    end if;
end process;


end Behavioral;
