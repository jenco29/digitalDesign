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

-- State declaration for main FSM
  TYPE state_type IS (INIT, A, AN, ANN, ANNN, ANNN_BYTE_IN, ANNN_BYTE_OUT1, ANNN_BYTE_OUT2, ANNN_DONE ,P, P_BYTE1, P_BYTE2, P_SPACE, P_INDEX1,P_INDEX2,P_INDEX3, LIST_INIT, LIST_PRINT1, LIST_PRINT2);  -- List your states here 	
  SIGNAL topCurState, topNextState: state_type;
    
    signal data_reg: std_logic_vector(7 downto 0);   -- data_reg: register to synchronously store byte from rx

    signal to_be_sent: std_logic_vector(7 downto 0); --to store the next byte to be sent to tx in hex
    signal ANNN_reg : BCD_ARRAY_TYPE(3 downto 0) := (others => "0000"); -- N registers
    
    signal nibble1, nibble2, ascii_prefix1, ascii_prefix2: std_logic_vector(3 downto 0) := "0000";
    signal int1, int2: natural := 0;
    signal peakStore, listStore: std_logic_vector(7 downto 0) := "00000000";
    
    signal ListCount, ANNN_byteCount,NNN : integer :=0;

    signal enSend, enSent, peakStored, listStored, NNNStored : boolean := false;

   
        -- constants of symbols in ASCII binary code
    constant lowerp : std_logic_vector (7 downto 0) := "01110000";
    constant upperp : std_logic_vector (7 downto 0) := "01010000";
    constant lowerl : std_logic_vector (7 downto 0) := "01101100";
    constant upperl : std_logic_vector (7 downto 0) := "01001100";
    constant lowera : std_logic_vector (7 downto 0) := "01100001";
    constant uppera : std_logic_vector (7 downto 0) := "01000001";
    constant space : std_logic_vector (7 downto 0) := "00100000";
    constant num_ascii : std_logic_vector (3 downto 0) := "0011";
    constant letter_ascii : std_logic_vector (3 downto 0) := "0100";


BEGIN

---------------store byte from rx--------------------------
rxData_In : process(clk)
--storing data value inputted on the clock edge
begin
    if rising_edge(clk) then
        if rxnow='1' then
            data_reg <= rxdata;
            rxdone <= '1';
        end if;
    end if;
end process;   


---------------int and nibble to ascii--------------------------
nibble_to_asc : process(clk)
--storing data value inputted on the clock edge
begin
    if int1 > 9 then
        ascii_prefix1 <= letter_ascii;
    else 
        ascii_prefix1 <= num_ascii;
    end if;
    
    if int2 = 0 then
        ascii_prefix2 <= "0010";  
    elsif int2 < 10 and int2 > 0 then
        ascii_prefix2 <= num_ascii;
    else 
        ascii_prefix2 <= letter_ascii;
    end if;

end process;
  -----------------------------------------------------
  
  
  combi_topNextState: PROCESS(topCurState, clk)
  BEGIN
    CASE topCurState IS
      WHEN INIT =>
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
        
    END CASE;
  END PROCESS; -- combi_nextState
  -----------------------------------------------------
  
  --processes on fsm states
state_logic : process(topCurState, clk)
begin
enSent <= false;  
peakStored <= false;
listStored <= false;
    case topCurState is
        
        when INIT =>


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
          
          int1 <= TO_INTEGER (UNSIGNED(nibble1));
          int2 <= TO_INTEGER (UNSIGNED(nibble1));


        when ANNN_BYTE_OUT1  =>
          to_be_sent(7 downto 4) <= ascii_prefix1; 
          to_be_sent(3 downto 0) <= nibble1;
          enSend <= true;

          
        when ANNN_BYTE_OUT2  =>
          to_be_sent(7 downto 4) <= ascii_prefix2; 
          to_be_sent(3 downto 0) <= nibble2; 
          enSend <= true;
          ANNN_byteCount <= ANNN_byteCount + 1;

          
        when ANNN_DONE => 
          start <= '0';    

        when P =>
          peakStore <= dataResults(3); 
          int1 <= TO_INTEGER (UNSIGNED(peakStore(3 downto 0)));
          int2 <= TO_INTEGER (UNSIGNED(peakStore(7 downto 4)));
          peakStored <= true;

          when P_BYTE1 =>

           to_be_sent <= ascii_prefix1 & peakStore(3 downto 0);           
            enSend <= true;
            
          when P_BYTE2 =>
           to_be_sent <= ascii_prefix2 & peakStore(7 downto 4);                    
            enSend <= true;
            
           when P_SPACE =>         
           to_be_sent <= space;          
            enSend <= true;
            
           when P_INDEX1 =>         
           to_be_sent <= num_ascii & maxIndex(0);          
            enSend <= true;
            
         when P_INDEX2 =>         
            to_be_sent <= num_ascii & maxIndex(1);          
            enSend <= true;
            
           when P_INDEX3 =>         
            to_be_sent <= num_ascii & maxIndex(2);
           enSend <= true;
           

        when LIST_INIT =>
          listStore <= dataResults(listCount); 
          int1 <= TO_INTEGER (UNSIGNED(listStore(3 downto 0)));
          int2 <= TO_INTEGER (UNSIGNED(listStore(7 downto 4)));
          listStored <= true;
            
        when LIST_PRINT1 =>          
          to_be_sent <= ascii_prefix1 & listStore(3 downto 0);
          enSend <= true;
                        
        when LIST_PRINT2 =>
           to_be_sent <= ascii_prefix2 & listStore(7 downto 4);
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
        if (topCurState = INIT) and (rxnow = '1') then
            txNow <= '1';
            txData <= rxData;
        elsif enSend = true then 
            txNow <= '1';
            txData <= to_be_sent;
            enSent <=true;         
        end if;      
     txNow <= '0';
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
