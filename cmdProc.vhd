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
  TYPE state_type IS (INIT, DATA_ECHO, INIT_BYTE, A, A_WAIT, AN, AN_WAIT, ANN, ANN_WAIT, ANNN, ANNN_BYTE_IN,ANNN_BYTE_OUT1_DONE,
  ANNN_BYTE_OUT2_DONE,SEND_SPACE, ANNN_DONE_CHECK, SEQ_DONE,ANNN_BYTE_COUNT, ANNN_BYTE_OUT1, ANNN_BYTE_OUT2, ANNN_DONE ,P, P_BYTE1,P_BYTE1_DONE,
  P_BYTE2, P_BYTE2_DONE, P_SPACE, P_INDEX1, P_INDEX1_DONE, P_INDEX2, P_INDEX2_DONE,P_INDEX3, LIST_INIT, LIST_PRINT1, LIST_PRINT1_DONE,LIST_SPACE, LIST_PRINT2, LIST_PRINT2_DONE, LIST_COUNT);  -- List your states here 	
  SIGNAL curState, nextState: state_type;
    
    signal data_reg, byte_reg: std_logic_vector(7 downto 0):= (others => '0');   -- data_reg: register to synchronously store byte from rx

    signal to_be_sent,sending: std_logic_vector(7 downto 0) := (others => '0'); --to store the next byte to be sent to tx in hex
    signal ANNN_reg : BCD_ARRAY_TYPE(2 downto 0); -- N registers
    
    signal nibble1, nibble2: std_logic_vector(3 downto 0):= (others => '0');
    signal peakStore, listStore: std_logic_vector(7 downto 0) := (others => '0');
    
    signal listCount, ANNN_byteCount,NNN,digitCount,index : integer :=0;

    signal enSend, enSent, peakStored, listStored, NNNStored,bytes_stored, byte_sent, results_stored, ANNN_end, newNNN : boolean := false;

    signal rxnow_reg, txdone_reg, dataReady_reg, seqDone_reg  : std_logic;
    signal maxIndex_reg : BCD_ARRAY_TYPE(2 downto 0) := (others => (others => '0'));
    signal dataResults_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) := (others => ( others => '0')); -- N registers
    -- byte store: register that stores full sequence of bytes from data processor
    signal byte_store : CHAR_ARRAY_TYPE(0 to SEQ_LENGTH + 5) := (others => ( others => '0'));

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

set_annn_end : process(curState, ANNN_byteCount,NNN)
--storing data value inputted on the clock edge
begin
            if curState = ANNN_DONE_CHECK and ANNN_byteCount > NNN-1 then
                ANNN_end<= true;
            else
                ANNN_end<= false;           
            end if;
end process; 

---------------store byte from rx--------------------------
set_to_be_sent : process(curState, nibble1,nibble2,listStore,peakStore,maxIndex_reg)
--storing data value inputted on the clock edge
begin
    if curState= ANNN_BYTE_OUT1 then    
          to_be_sent <= to_ascii(nibble1);
          
     elsif curState= ANNN_BYTE_OUT2   then    
          to_be_sent <= to_ascii(nibble2);
          
     elsif curState= SEND_SPACE or curState = P_SPACE or curState = LIST_SPACE  then    
          to_be_sent <= space;
          
     elsif   curState= P_BYTE1   then    
          to_be_sent <= to_ascii(peakStore(7 downto 4));
          
     elsif   curState= P_BYTE2   then    
          to_be_sent <= to_ascii(peakStore(3 downto 0));
             
      elsif   curState= P_INDEX1   then    
          to_be_sent <= to_ascii(maxIndex_reg(0));
           
                elsif   curState= P_INDEX2   then    
          to_be_sent <= to_ascii(maxIndex_reg(1));
          
                elsif   curState= P_INDEX3   then    
          to_be_sent <= to_ascii(maxIndex_reg(2));   
          
                          elsif   curState= LIST_PRINT1   then    
          to_be_sent <= to_ascii(listStore(7 downto 4));   
          
                          elsif   curState= LIST_PRINT2   then    
          to_be_sent <= to_ascii(listStore(3 downto 0));   
     else
     to_be_sent<= "00000000";
          
    end if;
end process;

-- process to store each incoming byte on the clock when data is to be read
store_byte : process(clk,curState,dataReady_reg)
begin
    if rising_edge(clk) then
    
    if curState = INIT then
        index <= 0;
    else
        if dataReady_reg='1' then
            byte_store(index) <= byte_reg;
            index<=index+1;           
        end if;       
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

reg_dataResults : process(clk,seqDone_reg)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            if seqDone_reg='1' then           
                dataResults_reg <= dataResults;
            end if;
    end if;
end process; 

reg_maxIndex : process(clk,seqDone_reg)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then   
            if seqDone_reg='1' then           
            maxIndex_reg <= maxIndex;
            end if;
    end if;
end process; 

set_results : process(clk,results_stored)
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

set_start : process(clk,curState,index,NNN)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then 
        if curState=ANNN THEN
         start<='1'; 
        elsif curState=ANNN_BYTE_IN then
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

reg_bytecount : process(clk,curState)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
                if curState = INIT then
                ANNN_byteCount<= 0;
                elsif curState=ANNN_BYTE_COUNT then   
                   ANNN_byteCount <= ANNN_byteCount +1;
                end if;
            end if;
end process; 

reg_listcount : process(clk,curState)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
                if curState = INIT then
                listCount <= 0;
                elsif curState=LIST_COUNT then   
                   listCount <= listCount +1;
                end if;
            end if;
end process; 

reg_seqDone : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then    
            seqDone_reg <= seqDone;
    end if;
end process; 

reg_rxdata : process(clk,rxnow_reg)
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
    if rising_edge(clk) then
              ANNN_reg(digitCount+1) <= data_reg(3 downto 0);
    end if;
end process;

SET_NUMWORDS : process(clk)
begin    
    if rising_edge(clk) then
          numwords_bcd(2) <= ANNN_reg(0);
          numwords_bcd(1) <= ANNN_reg(1);
          numwords_bcd(0) <= ANNN_reg(2);   
    end if;
end process;

reg_byte : process(clk,dataReady)
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

reg_rxnow : process(clk,rxnow_reg,rxnow)
begin
    if rising_edge(clk) then
        rxnow_reg <= rxnow;
    end if;
end process;

  
  
  combi_nextState: PROCESS(curState,listCount, clk, data_reg, enSent, rxnow_reg,rxnow,NNNStored,bytes_stored,seqDone_reg,results_stored,ANNN_end,peakStored)
  BEGIN
    CASE curState IS
      WHEN INIT =>
        IF rxnow_reg = '1' THEN 
                  nextState <= INIT_BYTE;
        ELSE
                  nextState <= INIT;
                              
        END IF;
        
         WHEN INIT_BYTE =>
        IF data_reg = lowera or data_reg = uppera THEN 
          nextState <= A;
        ELSIF data_reg = lowerp or data_reg = upperp THEN 
          nextState <= P;
        ELSIF data_reg = lowerl or data_reg = upperl THEN 
          nextState <= LIST_INIT;
        ELSE
          nextState <= INIT;
        END IF;

        
        
      WHEN A =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='1') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            nextState <= A_WAIT;
        ELSE 
            nextState <= A;
        END IF;
        
         WHEN A_WAIT =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='0') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            nextState <= AN;
        ELSE
            nextState <= A_WAIT;
        END IF;
             
              WHEN AN =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='1') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            nextState <= AN_WAIT;
        ELSE
            nextState <= AN;
        END IF;
 
          WHEN AN_WAIT =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='0') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            nextState <= ANN;
        ELSE
            nextState <= AN_WAIT;
        END IF;
                             
            WHEN ANN =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='1')and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            nextState <= ANN_WAIT;
        ELSE
            nextState <= ANN;
        END IF;
 
          WHEN ANN_WAIT =>
        IF (data_reg(7 downto 4) = num_ascii) and (rxNow_reg='0') and ((TO_INTEGER(UNSIGNED(data_reg))) > 47) and ((TO_INTEGER(UNSIGNED(data_reg))) < 58) THEN 
            nextState <= ANNN;
        ELSE
            nextState <= ANN_WAIT;
        END IF;
                      
         WHEN ANNN =>
        IF NNNStored = true THEN 
            nextState <= ANNN_BYTE_IN;
        ELSE
            nextState <= ANNN;
        END IF;
                      
        WHEN ANNN_BYTE_IN =>
        IF bytes_stored = true and seqDone_reg='1' THEN 
            nextState <= SEQ_DONE;
        ELSE
           nextState <= ANNN_BYTE_IN;
        END IF;
        
                WHEN SEQ_DONE =>
        IF results_stored = true THEN 
            nextState <= ANNN_BYTE_OUT1;
        ELSE
           nextState <= SEQ_DONE;
        END IF;
        
                      
        WHEN ANNN_BYTE_OUT1 =>
        IF enSent = true THEN 
            nextState <= ANNN_BYTE_OUT1_DONE;
        ELSE
           nextState <= ANNN_BYTE_OUT1_DONE;
        END IF;
        
                WHEN ANNN_BYTE_OUT1_DONE =>
        IF enSent = true THEN 
            nextState <= ANNN_BYTE_OUT2;
        ELSE
           nextState <= ANNN_BYTE_OUT1_DONE;
        END IF;
              
        WHEN ANNN_BYTE_OUT2 =>
        IF enSent = true THEN 
            nextState <= ANNN_BYTE_OUT2_DONE;
        ELSE
           nextState <= ANNN_BYTE_OUT2_DONE;
        END IF;        
        
                WHEN ANNN_BYTE_OUT2_DONE =>
        IF enSent = true THEN 
            nextState <= SEND_SPACE;
        ELSE
           nextState <= ANNN_BYTE_OUT2_DONE;
        END IF; 
        
                WHEN SEND_SPACE =>
        IF enSent = true THEN 
            nextState <= ANNN_BYTE_COUNT;
        ELSE
           nextState <= SEND_SPACE;
        END IF; 
                
                 WHEN ANNN_BYTE_COUNT =>
        IF enSent = true THEN 
            nextState <= ANNN_DONE_CHECK;
        ELSE
           nextState <= ANNN_DONE_CHECK;
        END IF;
                  
        
        WHEN ANNN_DONE_CHECK =>
        IF ANNN_end=true THEN
            nextState <= INIT;
            ELSE
            nextState <= ANNN_BYTE_OUT1;
        END IF;
             
             
         WHEN P =>
            IF peakStored = true THEN 
                nextState <= P_BYTE1;
            ELSE
               nextState <= P;
             END IF;
             
          WHEN P_BYTE1 =>
            IF enSent = true THEN 
                nextState <= P_BYTE1_DONE;
            ELSE
               nextState <= P_BYTE1_DONE;                
            END IF;
            
                   WHEN P_BYTE1_DONE =>
            IF enSent = true THEN 
                nextState <= P_BYTE2;
            ELSE
               nextState <= P_BYTE1_DONE;                
            END IF;
             
         WHEN P_BYTE2 =>
            IF enSent = true THEN 
                nextState <= P_BYTE2_DONE;
             ELSE
               nextState <= P_BYTE2_DONE;               
             END IF;
             
             WHEN P_BYTE2_DONE =>
            IF enSent = true THEN 
                nextState <= P_SPACE;
            ELSE
               nextState <= P_BYTE2_DONE;                
            END IF;
             
         WHEN P_SPACE =>
            IF enSent = true THEN 
                nextState <= P_INDEX1;
            ELSE
               nextState <= P_SPACE;
             END IF;                     
             
        WHEN P_INDEX1 =>
            IF enSent = true THEN 
                nextState <= P_INDEX1_DONE;
            ELSE
               nextState <= P_INDEX1_DONE;
             END IF; 
             
                 WHEN P_INDEX1_DONE =>
            IF enSent = true THEN 
                nextState <= P_INDEX2;
            ELSE
               nextState <= P_INDEX1_DONE;
             END IF; 
                 
         WHEN P_INDEX2 =>
            IF enSent = true THEN 
                nextState <= P_INDEX3;
            ELSE
               nextState <= P_INDEX2;
             END IF;   
             
               WHEN P_INDEX2_DONE =>
            IF enSent = true THEN 
                nextState <= P_INDEX3;
            ELSE
               nextState <= P_INDEX2_DONE;
             END IF;      
                   
       WHEN P_INDEX3 =>
            IF enSent = true THEN 
                nextState <= INIT;
            ELSE
               nextState <= P_INDEX3;
             END IF; 
             
        
       WHEN LIST_INIT =>
        IF listCount = 7 THEN 
          nextState <= INIT;
        ELSE
          nextState <= LIST_PRINT1;      
        END IF;
                     
      WHEN LIST_PRINT1 =>
            IF enSent = true THEN 
                nextState <= LIST_PRINT1_DONE;
        ELSE
                nextState <= LIST_PRINT1_DONE;    
             END IF;
             
             WHEN LIST_PRINT1_DONE =>
            IF enSent = true THEN 
                nextState <= LIST_PRINT2;
        ELSE
                nextState <= LIST_PRINT1_DONE;    
             END IF;        
        
       WHEN LIST_PRINT2 =>
            IF enSent = true THEN 
                nextState <= LIST_PRINT2_DONE;
           ELSE
                nextState <= LIST_PRINT2_DONE;
             END IF;
             
                    WHEN LIST_PRINT2_DONE =>
            IF enSent = true THEN 
                nextState <= LIST_SPACE;
           ELSE
                nextState <= LIST_PRINT2_DONE;
             END IF;
             
            WHEN LIST_SPACE =>
            IF enSent = true THEN 
                nextState <= LIST_COUNT;
        ELSE
                nextState <= LIST_SPACE;    
             END IF;
      
            WHEN LIST_COUNT =>

           IF listCount > 6 THEN
                nextState <= INIT;
           ELSE 
                nextState <= LIST_PRINT1;
             END IF;
                         
             
             WHEN others =>
                nextState <= INIT;
                    

        
    END CASE;
  END PROCESS; -- combi_nextState
  -----------------------------------------------------
  
  set_enSend : process(curState)
--storing data value inputted on the clock edge
begin
            if curState = ANNN_BYTE_OUT1 or curState = ANNN_BYTE_OUT2 
            or curState = SEND_SPACE or curState = P_BYTE1 or curState = P_SPACE
            or curState = P_INDEX1 or curState = P_INDEX2  or curState = P_INDEX3 
            or curState = LIST_PRINT1  or curState = LIST_PRINT2   or curState = LIST_SPACE  then
                enSend<= true;
            else
                enSend<= false;           
            end if;
end process; 

  set_digitCount : process(curState)
--storing data value inputted on the clock edge
begin
            if curState = AN_WAIT  then
          digitCount <= 0;
          elsif curState = ANN_WAIT then
          digitCount <= 1;
          elsif curState = ANNN then
          digitCount <= 2;           
            else
          digitCount <= 0;           
            end if;
end process; 


  set_NNN : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
         NNN <= ( (TO_INTEGER(UNSIGNED(ANNN_reg(0)))*100) + (TO_INTEGER(UNSIGNED(ANNN_reg(1)))*10) + (TO_INTEGER(UNSIGNED(ANNN_reg(2)))));  

    end if;
end process; 

  set_NNNStored : process(curState)
--storing data value inputted on the clock edge
begin
            if curState = ANNN  then
          NNNStored <= true;
          ELSE
           NNNStored <= false;         
          END IF;
end process; 
 
   set_byte_sent : process(curState)
--storing data value inputted on the clock edge
begin
         if curState = SEND_SPACE  then
          byte_sent <= true;
          ELSE
           byte_sent <= false;         
          END IF;
end process;   

   set_peakStore : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
          peakStore <= dataResults_reg(3); 
   END IF;
end process; 
  
   set_peakStored : process(curState)
--storing data value inputted on the clock edge
begin
         if curState = P  then
          peakStored <= true;
          ELSE
           peakStored <= false;         
          END IF;
end process; 

   set_listStored: process(curState)
--storing data value inputted on the clock edge
begin
         if curState = LIST_INIT  then
          listStored <= true;
          ELSE
           listStored <= false;         
          END IF;
end process; 

   set_listStore: process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
            listStore <= dataResults_reg(listCount);                
          END IF;
end process; 
     

  ----------------output to tx--------------------------

txData_Out : process(clk,curState,enSent)
begin
    if rising_edge(clk) then
        if rxnow_reg = '1' then --ADD AND RXNOW='1' BACK IN PLSSSSSSSSSSSSSSSS              
            txNow <= '1';
        elsif curState = ANNN_BYTE_OUT1 and enSent=true then
            txNow <= '1';                  
        elsif curState = ANNN_BYTE_OUT2 and enSent=true then 
             txNow <= '1';    
         elsif curState = LIST_PRINT1 and enSent=true then 
             txNow <= '1';
        elsif curState = LIST_PRINT2 and enSent=true then 
             txNow <= '1';                        
                 elsif curState = P_BYTE1 and enSent=true then 
             txNow <= '1';
        elsif curState = P_BYTE2 and enSent=true then 
             txNow <= '1';   
                 elsif curState = P_INDEX1 and enSent=true then 
             txNow <= '1'; 
                     elsif curState = P_INDEX2 and enSent=true then 
             txNow <= '1'; 
                     elsif curState = P_INDEX3 and enSent=true then 
             txNow <= '1'; 
       else 
          txNow <= '0';        
        end if;      
    end if;
end process;

txData_Out2 : process(clk,txDone_reg)
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

txData_Out3 : process(clk,curState)
begin
    if rising_edge(clk) then
            if curState = ANNN_BYTE_COUNT or curState = ANNN_DONE_CHECK or curState=ANNN_BYTE_OUT1 or curState=ANNN_BYTE_OUT2 or curState = LIST_COUNT
            or curState= SEND_SPACE or curState = ANNN_BYTE_OUT1_DONE or curState = ANNN_BYTE_OUT2_DONE or curState = LIST_PRINT1 or curState = LIST_PRINT2
            or curState= LIST_PRINT1_DONE or curState= LIST_PRINT2_DONE or curState= LIST_SPACE or curState= P_BYTE1 or curState= P_BYTE1_DONE or curState= P_BYTE2
            or curState= P_BYTE2_DONE or curState= P_INDEX1 or curState= P_INDEX1_DONE or curState= P_INDEX2 or curState= P_INDEX2_DONE or curState = P_SPACE or curState= P_INDEX3  then   
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
        curState <= INIT;
    elsif rising_edge(clk) then
        curState <= nextState;
    end if;
end process;


end Behavioral;
