
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

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
      rxnow: in std_logic;
      rxData: in std_logic_vector (7 downto 0);
      txData: out std_logic_vector (7 downto 0);
      rxdone: out std_logic;
      ovErr: in std_logic;
      framErr:	in std_logic;
      txnow: out std_logic;
      txdone: in std_logic;
      start: out std_logic;
      numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
      dataReady: in std_logic;
      byte: in std_logic_vector(7 downto 0);
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
end cmdProc;

architecture Behavioral of cmdProc is

begin
//Next state logic
next_State_process : process (CLK)
begin
	if (rising_edge(CLK)) then
		case State is 
		when INIT =>
		txdone <= '0';
		txNow <= '0';
		start <= '0';
		numWords_bcd <= '0';                                      
			if (valid = '1') then
				State <= SENDBYTE;
			end if;
		  if (valid = '1') and (rxData = '01000001' or '01100001') then
        when CHECKNNN =>
		  txdone <= '1';
		  if rxdata = integer		  
		      counterN <= counterN + 1
		      if counterN = 1 then
		          reg1 = N
		      else if counterN = 2 then
		          reg2 = N
		      else if counterN = 3 then
		          reg3 = N
		      end if;
		  end if;
		when START_DP =>
		  numWords <= reg1 + reg2 + reg3
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


end Behavioral;
