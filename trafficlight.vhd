LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- [0]
                                    "1111001" WHEN "00001",    -- [1]
                                    "0100100" WHEN "00010",    -- [2]      +---- a ----+
                                    "0110000" WHEN "00011",    -- [3]      |           |
                                    "0011001" WHEN "00100",    -- [4]      |           |
                                    "0010010" WHEN "00101",    -- [5]      f           b
                                    "0000010" WHEN "00110",    -- [6]      |           |
                                    "1111000" WHEN "00111",    -- [7]      |           |
                                    "0000000" WHEN "01000",    -- [8]      +---- g ----+
                                    "0010000" WHEN "01001",    -- [9]      |           |
                                    "0001000" WHEN "01010",    -- [A]      |           |
                                    "0000011" WHEN "01011",    -- [b]      e           c
                                    "0100111" WHEN "01100",    -- [c]      |           |
                                    "0100001" WHEN "01101",    -- [d]      |           |
                                    "0000110" WHEN "01110",    -- [E]      +---- d ----+
                                    "0001110" WHEN "01111",    -- [F]
                                    "1111111" WHEN OTHERS;     -- [ ]

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab4 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw		 : IN  STD_LOGIC_VECTOR(17 downto 14);

      ledr       : OUT STD_LOGIC_VECTOR(17 downto 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2, hex4, hex6 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab4;

ARCHITECTURE SimpleCircuit OF Lab4 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL TenHzModCLK:  STD_LOGIC; -- modulus 10hz clock
   	
   SIGNAL bin_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL mod_counter1:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- mod counter for 10Hz;
   SIGNAL mod_counter2:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   --CONSTANT ten_hz_val : UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := "0001001100010010110100000"; -- DEC value to output a 10Hz clock
   CONSTANT ten_hz_val : UNSIGNED(CLK_DIV_SIZE DOWNTO 0) := "10111110101111000010000100"; -- SIMULATION
 --CONSTANT one_hz_val : UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := "1011111010111100001000000"; -- DEC value to output a 1Hz clock
  CONSTANT one_hz_val : UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := "0000000000000000000000101"; -- SIMULATION
   TYPE STATES IS (GREEN_FLASH_NS, GREEN_SOLID_NS, RED_FLASH_NS, GREEN_FLASH_EW, GREEN_SOLID_EW, RED_FLASH_EW);   -- list all the STATES
   SIGNAL state: STATES;
   SIGNAL next_state:  STATES;                 -- current and next state signals of type STATE
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to displa on seven-segment
   SIGNAL waitNS_counter: UNSIGNED(3 DOWNTO 0);
   SIGNAL waitEW_counter: UNSIGNED(3 DOWNTO 0);
   
   SIGNAL checkNS : STD_LOGIC;
   SIGNAL checkEW : STD_LOGIC;
   TYPE boolean IS (false,true);
   SIGNAL temp : boolean :=false;
  
   ---------------------------------------------------------------------

----- MODULUS COUNTER FOR 10HZ AND 1HZ
BEGIN      
   ModCLK: PROCESS(clock_50) 
   BEGIN
    -- IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
      -- IF (mod_counter1 =  ten_hz_val) THEN       -- half period
       --  TenHzModCLK <= NOT TenHzModCLK;                 -- toggle, i.e, make it blink
       --  mod_counter1 <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
  --  ELSE
       --    mod_counter1 <= mod_counter1 + 1; 
     -- end IF;
   -- END IF;
      IF (rising_edge(clock_50)) THEN   
        IF (mod_counter2 = one_hz_val) THEN
	    OneHzModCLK <= NOT OneHzModCLK; -- toggle ; make it blink
            mod_counter2 <= to_unsigned(0,CLK_DIV_SIZE);
         ELSE
            mod_counter2 <= mod_counter2 + 1;
       END IF;
    END IF;
   LEDG(0) <= TenHzModCLK;
   LEDG(1) <= OneHzModCLK;
END PROCESS;
TenHzModCLK <= clock_50;
-------------------------------
----------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
 FSM: PROCESS(TenHzModCLK,state)
   BEGIN
      CASE state IS 
----------------------------------GREEN FLASH NS -----------------------------------
		   WHEN GREEN_FLASH_NS => -- NS Green light flashing
		        state_number <= "0000";
				ledr(0) <= '1';        -- EW RED LIGHT ON.
				ledg(7) <= '0';
				ledg(8) <= TenHzModCLK;
				LEDR(11) <= '0';
				checkNS <= '0';
				
				IF(SW(14) = '1' AND SW(17) = '1' AND SW(16) = '0' ) THEN
				checkEW <= '1';
				else
				checkEW <= '0';
				end if;
				
				IF(sw(17) = '0' and state_counter = "0010") THEN -- IF DAY MODE
	                    next_state <= GREEN_SOLID_NS; -- move to next state if counter reaches 2 ( 2 being amount of seconds)
				ELSIF (sw(17) ='1' AND sw(16) = '0' AND sw(14) ='0' and state_counter = "0010") THEN -- If night mode and DEFAULT side is ns and no car
					next_state <= GREEN_SOLID_NS; -- move to next state if counter reaches 2 ( 2 being amount of seconds)
				ELSIF(sw(17) ='1' AND SW(16) ='0' AND sw(14)= '1' and state_counter = "0010") then --AND SW(15) ='0' AND state_counter = "0010" ) then
					LEDG(8) <= '0';
					next_state <= GREEN_FLASH_EW;	
				ELSIF(sw(17) = '1' AND sw(16) = '1' AND sw(15) ='1' and state_counter = "0010") THEN -- if default side is NS and car on either side
					next_state <= GREEN_SOLID_NS;	
				ELSIF(sw(17) = '1' AND sw(16) = '1' AND sw(15) ='0' and state_counter = "0010") THEN -- if default side is NS and car on either side
					next_state <= GREEN_FLASH_EW;
		ELSE
		 next_state <= GREEN_FLASH_NS;
		 END IF;
-------------------------------GREEN SOLID NS --------------------------
         WHEN GREEN_SOLID_NS => 
            state_number <= "0001";
			ledg(7) <= '0';	
			ledr(0) <= '1';	-- EW RED LIGHT STILL ON				
			LEDG(8) <= '1';	
			checkNS <= '0';
			
			IF(SW(14) = '1' AND SW(17) = '1' AND SW(16) = '0' ) THEN
				checkEW <= '1';
				else
				checkEW <= '0';
		end if; 
         IF(SW(17) ='0' AND state_counter = "0100") THEN 
               next_state <= RED_FLASH_NS;
         ELSIF (sw(17) ='1' AND sw(16) = '0' AND (sw(14) ='0' AND SW(15)= '0') and state_counter = "0100") then-- if NM ON, DS NS, AND NO CAR
               next_state <= RED_FLASH_NS;
         ELSIF(sw(17) = '1' AND sw(16) = '0' AND (sw(14) ='1' OR SW(15) ='1') and state_counter= "0100") THEN --if night mode
				next_state <= RED_FLASH_NS;
		ELSIF(sw(17) = '1' AND sw(16) = '0' AND (sw(14) ='1') and state_counter= "0100") THEN --if night mode
				next_state <= RED_FLASH_NS;
		 ELSIF(sw(17) = '1' AND sw(16) = '1' AND (sw(14) ='1' OR SW(15) ='1') and state_counter= "0100") THEN --if night mode
				next_state <= RED_FLASH_NS;
		ELSIF(sw(17) = '1' AND sw(16) = '1' AND (sw(14) ='0' OR SW(15) ='0') and state_counter= "0100") THEN --if night mode
				next_state <= RED_FLASH_NS;			
	     else
				next_state <= GREEN_SOLID_NS;
          END IF;
----------------------------- RED FLASH NS --------------------------    
         WHEN RED_FLASH_NS =>
         	state_number <= "0010";
            LEDR(11) <= TenHzModCLK;
            LEDG(8) <= '0';
            LEDR(0) <= '1';     -- EW RED LIGHT STILL ON
            IF(SW(14) = '1' AND SW(17) = '1' AND SW(16) = '0' ) THEN
				checkEW <= '1';
				else
				checkEW <= '0';
				end if;
         IF(SW(17) = '0' and state_counter = "0010") THEN
               next_state <= GREEN_FLASH_EW;
         ELSIF (sw(17) ='1' AND sw(16) = '0' AND (sw(14) ='0' AND SW(15) = '0') and state_counter = "0010")	THEN -- if NM ON, DS NS, AND NO CAR				
               next_state <= GREEN_FLASH_NS;
         ELSIF(sw(17) = '1' AND sw(16) = '0' AND SW(15) ='1' and state_counter = "0010") THEN -- if default side is NS and car on either side
					next_state <= GREEN_FLASH_NS;
		ELSIF(sw(17) = '1' AND sw(16) = '0' AND SW(14) ='1' and state_counter = "0010") THEN -- if default side is NS and car on either side
					next_state <= GREEN_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '1' AND sw(14) ='1' and state_counter = "0010") THEN
					next_state <= GREEN_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '1' AND sw(15) ='1' and state_counter = "0010") THEN
					next_state <= GREEN_FLASH_NS;
		ELSIF(sw(17) = '1' AND sw(16) = '1' AND (sw(14) ='0' AND SW(15) = '0')and state_counter = "0010") THEN
					next_state <= GREEN_FLASH_EW;							
			else
					next_state <= RED_FLASH_NS;
		END IF;
 ----------------------------- GREEN FLASH EW --------------------------    
         WHEN GREEN_FLASH_EW =>
				state_number <= "0011";
				ledr(0) <= '0';   
				ledr(11) <= '1';        
				ledg(7) <= TenHzModCLK;
				checkEW <= '0';
		IF(SW(15) = '1' AND SW(17) = '1' AND SW(16) = '1' ) THEN
				checkNS <= '1';
		else
				checkNS <= '0';
	    end if;
	    
			IF(SW(17)='0' and state_counter = "0010") THEN
					next_state <= GREEN_SOLID_EW;
			ELSIF (sw(17) ='1' AND SW(16) ='1' AND SW(15) ='0' AND state_counter = "0010") then -- IF NM ON, DS EW, NO CAR ON no
					next_state <= GREEN_SOLID_EW;
			ELSIF(sw(17) = '1' AND sw(16) = '1' AND sw(15) ='1' AND state_counter = "0010") THEN -- IF NM ON, DS EW, AND CAR YES
					next_state <= GREEN_SOLID_EW;
			ELSIF(sw(17) = '1' AND sw(16) = '0' AND sw(14) ='1' AND state_counter = "0010") THEN -- IF NM ON, DS NS, AND CAR YES
					next_state <= GREEN_SOLID_EW;
			ELSIF(sw(17) = '1' AND sw(16) = '0' AND state_counter = "0010") THEN -- IF NM ON, DS NS, AND CAR no
					next_state <= GREEN_FLASH_NS;		
			ELSE 	
					next_state <= GREEN_FLASH_EW;
			END IF;
------------------------------- GREEN SOLID EW --------------------------
         WHEN GREEN_SOLID_EW =>
			state_number <= "0100";
			ledr(0) <= '0';
			ledg(7) <= '1'; 	
			ledr(11) <= '1';	--NS RED LIGHT ON
			checkEW <= '0';
			IF(SW(15) = '1' AND SW(17) = '1' AND SW(16) = '1' ) THEN
				checkNS <= '1';
				else
				checkNS <= '0';
				end if;	
		IF (sw(17) ='1' AND SW(16) ='1' AND SW(15) ='0' AND state_counter = "0100") then -- IF NM ON, DS EW, NO CAR ON NS		
               next_state <= RED_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '1' AND sw(15) ='1' and state_counter = "0100") THEN -- IF NM ON, DS EW, AND CAR YES
					next_state <= RED_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '0' AND SW(14) ='1' and state_counter = "0100") THEN -- IF NM ON, DS EW, AND CAR YES
					next_state <= RED_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '0' AND SW(14) ='0' and state_counter = "0100") THEN -- IF NM ON, DS EW, AND CAR YES
					next_state <= RED_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '0' and state_counter = "0100") THEN -- IF NM ON, DS EW, AND CAR YES
					next_state <= GREEN_FLASH_NS;											
		ELSIF(SW(17) = '0' and state_counter = "0100") THEN
               next_state <= RED_FLASH_EW;
            ELSE
               next_state <= GREEN_SOLID_EW;
            END IF;   
-------------------------------- RED FLASH EW ------------------------
         WHEN RED_FLASH_EW =>
         state_number <= "0101";
            LEDG(8) <= '0';
            ledg(7) <= '0';
            LEDR(0) <= TenHzModCLK;
            ledr(11) <= '1';
            IF(SW(15) = '1' AND SW(17) = '1' AND SW(16) = '1' ) THEN
				checkNS <= '1';
				else
				checkNS <= '0';
				end if;
		IF (sw(17) ='1' AND SW(16) ='1' AND SW(15) ='0' AND state_counter = "0010") then -- IF NM ON, DS EW, NO CAR ON NS
               next_state <= GREEN_FLASH_EW;
        ELSIF (sw(17) ='1' AND SW(16) ='0' AND SW(15) ='0' AND state_counter = "0010") then -- IF NM ON, DS EW, NO CAR ON NS
               next_state <= GREEN_FLASH_EW;
        ELSIF(sw(17) = '1' AND sw(16) = '0' and state_counter ="0010") THEN -- IF NM ON, DS EW, AND CAR ON NS
				 next_state <= GREEN_FLASH_NS;
	    ELSIF(sw(17) = '1' AND sw(16) = '0' AND SW(14) ='1' and state_counter ="0010") THEN -- IF NM ON, DS ns, AND CAR YES on EW
				 next_state <= GREEN_FLASH_EW;
		ELSIF(sw(17) = '1' AND sw(16) = '0' AND SW(14) ='0' and state_counter ="0010") THEN -- IF NM ON, DS ns, AND CAR YES on EW
				 next_state <= GREEN_FLASH_EW;  
		ELSIF(SW(17) = '0' and state_counter = "0010") THEN
				 next_state <= GREEN_FLASH_NS; 
		ELSIF(sw(17) ='1' AND SW(16) ='1' AND state_counter = "0010") THEN
				next_state <= GREEN_FLASH_EW;
			ELSE
				next_state <= RED_FLASH_EW;
		  END IF;
      END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
 SeqLogic: PROCESS(TenHzModCLK,OneHzModCLK) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(OneHzModCLK)) THEN 
            state_counter <= state_counter +1;            -- on the rising edge of clock the current state is updated with next state
         IF(state /= next_state) THEN
		    state_counter <= "0000";
			  -- reset the state counter everytime to get a correct timing estimation
		 END IF;
		 if(checkNS = '1' AND sw(15) = '1') THEN
				waitNS_counter <= waitNS_counter + 1;
		 ELSE
				waitNS_counter <= "0000";
	     END IF;
		 IF(checkEW = '1' AND sw(14) = '1') THEN
				waitEW_counter <= waitEW_counter + 1;
		 ELSE
				waitEW_counter <= "0000";
		 END IF;
		  state <= next_state;
		 
      END IF;
END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S1: SevenSegment PORT MAP( std_logic_vector(waitNS_counter), '0', hex4 );
   D7S2: SevenSegment PORT MAP( std_logic_vector(waitEW_counter), '0', hex6 );
   D7S0: SevenSegment PORT MAP( state_number, '0', hex2 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex0 );
   
   ledr(17 downto 14) <= STD_LOGIC_VECTOR(state_counter);
   ledr(10 downto 7) <= STD_LOGIC_VECTOR(waitNS_counter);
   ledr(6 downto 3) <= STD_LOGIC_VECTOR(waitEW_counter);
   ledg(6 downto 3) <= STD_LOGIC_VECTOR(state_number);
END SimpleCircuit;
