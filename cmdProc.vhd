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
  TYPE state_type IS (INIT, DATA_ECHO, INIT_BYTE, A, A_WAIT, AN, AN_WAIT, ANN, ANN_WAIT, ANNN,ANNN_BYTE_OUT1_DONE, ANNN_BYTE_IN, ANNN_DONE_CHECK, SEQ_DONE,ANNN_BYTE_COUNT, ANNN_BYTE_OUT1, ANNN_BYTE_OUT2, ANNN_DONE ,P, P_BYTE1, P_BYTE2, P_SPACE, P_INDEX1,P_INDEX2,P_INDEX3, LIST_INIT, LIST_PRINT1, LIST_PRINT2);  -- List your states here 	
  SIGNAL topCurState, topNextState: state_type;
    
    signal data_reg, byte_reg: std_logic_vector(7 downto 0);   -- data_reg: register to synchronously store byte from rx

    signal to_be_sent,sending: std_logic_vector(7 downto 0); --to store the next byte to be sent to tx in hex
    signal ANNN_reg : BCD_ARRAY_TYPE(2 downto 0); -- N registers
    
    signal nibble1, nibble2: std_logic_vector(3 downto 0);
    signal peakStore, listStore: std_logic_vector(7 downto 0);
    
    signal ListCount, ANNN_byteCount,NNN,digitCount,index,ANNN_NIBBLECount : integer :=0;

    signal enSend, enSent, peakStored, listStored, NNNStored,bytes_stored, byte_sent, byte_done, results_stored, ANNN_end,nib : boolean := false;

    signal rxnow_reg, txdone_reg, dataReady_reg, seqDone_reg  : std_logic;
    signal maxIndex_reg : BCD_ARRAY_TYPE(2 downto 0);
    signal dataResults_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- N registers
    -- byte store: register that stores full sequence of bytes from data processor
signal byte_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH + 5);

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

-- process to store each incoming byte on the clock when data is to be read
store_byte : process(clk)
begin
    if rising_edge(clk) then
        if dataReady_reg='1' then
            byte_store(index) <= byte_reg;
            index<=index+1;
        end if;
    end if;
end process; 

reg_dataReady : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            dataReady_reg <= dataReady;
    end if;
end process; 

reg_dataResults : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            dataResults_reg <= dataResults;
    end if;
end process; 

reg_maxIndex : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            maxIndex_reg <= maxIndex;
    end if;
end process; 

set_results : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
        if seqDone_reg ='1'    then
            results_stored <= true;
        else
            results_stored <= false;
        end if;
    end if;
end process; 

set_start : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then 
        if topCurState=ANNN THEN
         start<='1'; 
        elsif topCurState=ANNN_BYTE_IN then
                   if index < NNN then
                                      start<='1'; 
                   end if;
        elsif index > NNN-1 then
           start<='0';
           bytes_stored<=true; 
        else 
          start<='0'; 
          bytes_stored<=false; 
        end if;
    end if;
end process; 

reg_bytecount : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk)and topCurState=ANNN_BYTE_COUNT then    
                   ANNN_byteCount <= ANNN_byteCount +1;
            end if;
end process; 

reg_NIBBLEcount : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk)and topCurState=ANNN_BYTE_OUT1_DONE then    
                   ANNN_NIBBLECount <= ANNN_NIBBLECount +1;
            end if;
end process; 

byteDone : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk)THEN
        IF index>NNN-1 then    
                   byte_done<=true;
        ELSE
                        byte_done<=false;
            end if;
   END IF;
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
            rxDone <= '1';
        else
            rxDone <= '0';
        end if;
    end if;
end process;

SET_ANN_REG : process(clk)
begin    
    if rising_edge(clk) and topCurState = INIT then
              ANNN_reg(0) <= "0000";
              ANNN_reg(1) <= "0000";
              ANNN_reg(2) <= "0000";
    else
              ANNN_reg(digitCount+1) <= data_reg(3 downto 0);
    end if;
end process;

SET_NUMWORDS_REG : process(clk)
begin    
    if rising_edge(clk) then
          numwords_bcd(2) <= ANNN_reg(0);
          numwords_bcd(1) <= ANNN_reg(1);
          numwords_bcd(0) <= ANNN_reg(2);   
    end if;
end process;

reg_byte : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
        if dataReady = '1' then
            byte_reg <= byte;
        end if;
          nibble1 <= byte_store(ANNN_byteCount)(7 downto 4); 
          nibble2 <= byte_store(ANNN_byteCount)(3 downto 0);
          
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
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='1') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= A_WAIT;
        ELSE 
            topNextState <= A;
        END IF;
        
         WHEN A_WAIT =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='0') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= AN;
        ELSE
            topNextState <= A_WAIT;
        END IF;
             
              WHEN AN =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='1') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= AN_WAIT;
        ELSE
            topNextState <= AN;
        END IF;
 
          WHEN AN_WAIT =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='0') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= ANN;
        ELSE
            topNextState <= AN_WAIT;
        END IF;
                             
            WHEN ANN =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='1')and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= ANN_WAIT;
        ELSE
            topNextState <= ANN;
        END IF;
 
          WHEN ANN_WAIT =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='0') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            topNextState <= ANNN;
        ELSE
            topNextState <= ANN_WAIT;
        END IF;
                      
         WHEN ANNN =>
        IF NNNStored = true THEN 
            topNextState <= ANNN_BYTE_IN;
        ELSE
            topNextState <= ANNN;
        END IF;
                      
        WHEN ANNN_BYTE_IN =>
        IF bytes_stored = true and seqDone_reg='1' THEN 
            topNextState <= SEQ_DONE;
        ELSE
           topNextState <= ANNN_BYTE_IN;
        END IF;
        
                WHEN SEQ_DONE =>
        IF results_stored = true THEN 
            topNextState <= ANNN_BYTE_COUNT;
        ELSE
           topNextState <= SEQ_DONE;
        END IF;
        
         WHEN ANNN_BYTE_COUNT =>
        IF enSent = true THEN 
            topNextState <= ANNN_BYTE_OUT1;
        ELSE
           topNextState <= ANNN_BYTE_OUT1;
        END IF;
                      
        WHEN ANNN_BYTE_OUT1 =>
        IF enSent = true THEN 
            topNextState <= ANNN_BYTE_OUT1_DONE;
        ELSE
           topNextState <= ANNN_BYTE_OUT1;
        END IF;
        
                WHEN ANNN_BYTE_OUT1_DONE =>
        IF enSent = true THEN 
            topNextState <= ANNN_BYTE_OUT2;
        ELSE
           topNextState <= ANNN_BYTE_OUT1_DONE;
        END IF;
              
        WHEN ANNN_BYTE_OUT2 =>
        IF enSent = true THEN 
            topNextState <= ANNN_DONE_CHECK;
        ELSE
           topNextState <= ANNN_BYTE_OUT2;
        END IF;        
                  
        
        WHEN ANNN_DONE_CHECK =>
        IF ANNN_end=true THEN
            topNextState <= INIT;
            ELSE
            topNextState <= ANNN_BYTE_COUNT;
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
                ANNN_end<= false;


        when A => 
          
        when AN_WAIT => 
          digitCount <= 0;
                        
          
        when ANN_WAIT => 
              digitCount <= 1;

                  
        when ANNN => --start proc
         NNN <= ( (TO_INTEGER(UNSIGNED(ANNN_reg(0)))*100) + (TO_INTEGER(UNSIGNED(ANNN_reg(1)))*10) + (TO_INTEGER(UNSIGNED(ANNN_reg(2)))));
         -- start <= '1'; 
          NNNStored <= true;
        
        when ANNN_BYTE_IN   =>            
          enSend <=false;
                    to_be_sent <= to_ascii(nibble1); 
          byte_sent <= false;
          
          
        when ANNN_BYTE_COUNT  =>
          to_be_sent <= to_ascii(nibble1); 
          enSend <= true;
                    byte_sent <= false;

        
         when ANNN_BYTE_OUT1_DONE  =>
                   to_be_sent <= to_ascii(nibble2); 
          enSend <= false;
         
        when ANNN_BYTE_OUT2  =>
          to_be_sent <= to_ascii(nibble2); 
          enSend <= true;
          byte_sent <= true;
          
        when ANNN_DONE_CHECK => 
                  enSend <= FALSE;
          to_be_sent <= to_ascii(nibble1); 

            if ANNN_byteCount > NNN-1 then
                ANNN_end<= true;
            end if;
        
        when P =>
          peakStore <= dataResults(3); 
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
        if rxnow_reg = '1'  then --ADD AND RXNOW='1' BACK IN PLSSSSSSSSSSSSSSSS              
            txNow <= '1';
        elsif topCurState = ANNN_BYTE_COUNT and enSend=true then
            txNow <= '1';                  
        elsif topCurState = ANNN_BYTE_OUT2 and enSend=true then 
             txNow <= '1';        
       else 
          txNow <= '0';        
        end if;      
    end if;
end process;

txData_Out2 : process(clk)
begin
    if rising_edge(clk) then
            txData <= sending;
            if txDone_reg = '1' then   
                  enSent <=true;  
            else
                  enSent <=false;  
            end if;
    end if;
end process;

txData_Out3 : process(clk)
begin
    if rising_edge(clk) then
            if topCurState = ANNN_BYTE_OUT1_DONE or topCurState = ANNN_BYTE_IN or topCurState = ANNN_BYTE_OUT1 or topCurState = ANNN_BYTE_OUT2 or topCurState = ANNN_BYTE_COUNT or topCurState = ANNN_DONE_CHECK then   
                  sending <=to_be_sent;  
            else
                  sending <=data_reg;  
            end if;
    end if;
end process;

  
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
