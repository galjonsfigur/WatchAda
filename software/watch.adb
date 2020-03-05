with Interfaces;                   use Interfaces;
with CPU;
with System.GCC_Builtins;
with TickCounts; use TickCounts;

with Shared;

procedure Watch is

   package Timer renames CPU.ta3;
   package Clock renames CPU.bc2;

subtype Digit is Interfaces.Unsigned_8 range 0 .. 9;
subtype cathodes is Interfaces.Unsigned_8 range 0 .. 3;
subtype Segments is unsigned_8;

-- Name significant port or register bits
Osc_Fault : Boolean renames CPU.cpu.ifg1_bits(1);

-- Port bit assignments
-- P2 : 0 => a, 1=> b, 2 => c, 3 => d, 4 => e, 5 => f 
--      (6=> xtal in, 7 => xtal out)
-- P1 : 0 => an1, 1 => an2, 2 => an3, 3 => an4
--      4 => g, 5 => dp, 6 => light sensor, 7 -> switch

Port1_off    : constant Unsigned_8  := 16#B0#;
-- experimental version : invert anode 2
-- Port1_off    : constant Unsigned_8  := 16#B4#;
-- For inverting anode drivers
--Port1_off    : constant Unsigned_8  := 16#BF#;
-- (4 => off, 5 => off, 7 => true, others => false);

--Set_DP_Mask  : constant Unsigned_8  := 16#DF#;
Set_DP_Mask  : constant Unsigned_8  := 16#20#;
-- (bit 5 on, all others off since this is a mask)

-- Lookup1 contains the port bit allocations to display each digit on port 1
-- Lookup2 contains the port bit allocations to display each digit on port 2
Lookup2 : constant array (Digit) of Segments :=
    (0 => 2#00111111#,	-- 0 = off, 1 = on
     1 => 2#00000110#,
     2 => 2#01011011#,
     3 => 2#01001111#, 
     4 => 2#01100110#,
     5 => 2#01101101#,
     6 => 2#01111101#,
     7 => 2#00000111#,
     8 => 2#01111111#,
     9 => 2#01101111#);  

Lookup1 : constant array (Digit) of Segments :=
    (0 => 2#10000000#,  -- 0 = on, 1 = off
     1 => 2#10000000#,
     2 => 2#10010000#,
     3 => 2#10010000#,
     4 => 2#10010000#,
     5 => 2#10010000#,
     6 => 2#10010000#,
     7 => 2#10000000#,
     8 => 2#10010000#,
     9 => 2#10010000#);


Select_Cathode : constant array(Cathodes) of Unsigned_8 := (8,4,2,1);
-- Translate anode number to the port bit value that drives it
-- To drive common anode displays directly (OK via high value resistor)
--Select_Cathode : constant array(Cathodes) of Unsigned_8 := (1,2,4,8);

--Select_Cathode array for inverting anode driver on Cathode 2
--Select_Cathode : constant array(Cathodes) of Unsigned_8 := (5,6,0,12);

--For inverting anode driver on all cathodes.
--Select_Cathode : constant array(Cathodes) of Unsigned_8 := (16#E#,16#D#,16#B#,16#7#);
--Select_Cathode : constant array(Cathodes) of Unsigned_8 := (16#7#,16#B#,16#D#,16#E#);
-- Lookup24_1 and Lookup24_2 will display "12:__" when TwentyFour is false,
-- or "24:__" when TwentyFour is true, for setting 12/24 hour mode.
Lookup24_2 : constant array (Boolean,Cathodes) of Segments :=
   (False => (3 => Lookup2(1), 2 => Lookup2(2), 1 => Lookup2(0), 0 => Lookup2(0)),
    True  => (3 => Lookup2(2), 2 => Lookup2(4), 1 => Lookup2(0), 0 => Lookup2(0)));

Lookup24_1 : constant array (Boolean,Cathodes) of Segments :=
   (False => (3 => Lookup1(1) xor Select_Cathode(3), 
              2 => Lookup1(2) xor Select_Cathode(2), 
              1 => Port1_off, 
              0 => Port1_off),
    True  => (3 => Lookup1(2) xor Select_Cathode(3), 
              2 => Lookup1(4) xor Select_Cathode(2), 
              1 => Port1_off, 
              0 => Port1_off));

-- Local variables for main subprogram
TwentyFour : Boolean := True;
PM         : Boolean := False;

procedure Setup_Timer is
begin
   -- Basic clock setup
   Clock.dcoctl  := 0;				-- lowest DCOCTL frequency
        -- ClkCtrl 1	Calibration	-- Aclk = XT1 LF (32768) / 8 = 4096, XT2 off
   Clock.bcsctl1 := CPU.calibration.calbc1_1mhz or Clock.xt2off or Clock.diva_3;
   Clock.dcoctl  := CPU.calibration.caldco_1mhz;
   Clock.bcsctl2 := Clock.selm_0 or Clock.divm_0; -- Mclk = DCO = 1MHz
   Clock.bcsctl3 := Clock.xcap_2;		-- 32kHz osc, 10 pf loading.
   -- Timer A3 setup
   Timer.ccr0    := OneMinute - 1;		-- 1024*60;	 ticks per minute
   Timer.tactl   := Timer.tassel_1 or Timer.id_2 or Timer.mc_1 or Timer.taclr; -- select ACLK/4 = 1024Hz, up, clear
   Timer.cctl0   := Timer.ccie;			-- enable capture/control 0 interrupt
   Timer.cctl1   := 0;				-- capture/control 1 used for display scanning
   Timer.cctl2   := 0;				-- capture/control 2 unused here
end Setup_Timer;

procedure Setup_Ports is
use CPU.cpu, CPU.port1_r;
begin
    p2dir_bits := (6 => false, others => true);
    p2sel  := unsigned_8(bit6 or bit7);
    p2out_bits  := (others => false);
    p1dir  := unsigned_8(255 and not (bit6 or bit7));
    p1sel  := 0;
    p1out  := Port1_off;
--  P1 bit 7 is the switch input
    p1ren  := unsigned_8(bit7);	-- pullup on bit 7
--  Each PxIFG flag must be reset with software.
    p1ifg  := 0;
    p1ies  := unsigned_8(bit7);		-- falling edge
    p1ie   := unsigned_8(bit7);		-- and enable
end Setup_Ports;

procedure Setup_CPU is
begin
    -- Disable watchdog
    CPU.wdt.wdtctl := CPU.wdt.wdtpw or CPU.wdt.wdthold;
    -- turn on oscillator
    System.GCC_Builtins.bic_status_register(CPU.cpu.oscoff);
    -- and enable interrupts!
    System.GCC_Builtins.eint;
end Setup_CPU;

procedure Enter_LPM3 is
begin
   System.GCC_Builtins.bis_status_register(CPU.cpu.lpm3);
end Enter_LPM3;

procedure display_on is
Temp : Unsigned_16 := Timer.tar + 2;
begin
   if Temp >= OneMinute then 
      Temp := 1;
   end if;
   Timer.ccr1    := Temp;
   Timer.cctl1   := Timer.ccie;	  -- enable display interrupt
end display_on;

procedure display_off is
begin
   CPU.port1_r.p1out := Port1_off;
   Timer.cctl1   := 0;		  -- disable display interrupt
end display_off;

-- Divide by 10 and remainder, avoiding expensive math library
procedure DivRem10(Units : in out Unsigned_8; Tens : out Unsigned_8 ) is
begin
   Tens := 0;
   while Units >= 10 loop
      Units := Units - 10;
      Tens := Tens + 1;
   end loop;
end DivRem10;

-- States for the time setting state machine
type SetState is (normal, set_24, set_hour, set_minute, set_second, last);

procedure Button_Pressed is
Ticks         : TickCount;
Button_Time   : TickCount := Timer.tar;
Pulse_Time    : TickCount;
Pressed       : Boolean := True;
Acted         : Boolean := False;
SetMode       : SetState := normal;
Port1_on      : Unsigned_8;
HrMin         : Unsigned_8;
Tens          : Unsigned_8;
TheDigit      : Digit;
Display_Ticks : TickCount := Timeout;
Cathode         : cathodes := 0;
begin
   -- TODO : measure light level BEFORE turning lights on!
   -- TODO : if easter egg button signal then run egg

   -- if plain button signal then display for time T
   display_on;

   -- Display scanning loop. Each iteration displays a digit for 2 ms; 
   -- the digit to display is indicated by Cathode (range 0 .. 3)
   while Display_Ticks > 0 loop
      Ticks         := Timer.tar and SecondMask;
      Display_Ticks := Display_Ticks - 1;

      -- Load and process hour count. This is unconditional because we
      -- need to set "PM" correctly, whichever digit is being displayed.
      HrMin := Shared.Hours;
      PM    := False;
      if not TwentyFour then
          if HrMin  >= 12 then
              PM    := True;
              HrMin := HrMin - 12; 
          end if;
          if HrMin = 0 then HrMin := 12; end if;
      end if;

      -- Cathodes 0,1 control the minute digits, so display minutes instead.
      if Cathode < 2 then
         HrMin := Shared.Minutes;
      end if;

      -- Split number (hour or minute) into tens and units
      DivRem10(HrMin,Tens);

      -- Select the correct digit to display
      case Cathode is
         when 0|2 => TheDigit := Digit(HrMin);
         when 1|3 => TheDigit := Digit(Tens);
      end case;

      -- turn anodes off first to prevent visible glitches
      CPU.port1_r.p1out := Port1_off;

      -- Write Port 2 directly.
      -- Port 1 controls flashing, DP and other features
      -- so load a variable and modify it before writing to port.
      if SetMode = set_24 then -- display 12 or 24 (flashing, below)
         CPU.port1_r.p2out := Lookup24_2(TwentyFour,Cathode); 
         Port1_on := Lookup24_1(TwentyFour,Cathode);  
      else
         CPU.port1_r.p2out := Lookup2(TheDigit); 
         Port1_on := Lookup1(TheDigit) xor Select_Cathode(Cathode); 
      end if;

      -- flash seconds indicator (currently DP of digit 2) 
      -- set PM indicator (currently DP of digit 0)
      if (Ticks < Half_Second and then Cathode = 2) or else (PM and then Cathode = 0) then
      --if (Ticks < Half_Second and Cathode = 2) or (PM and Cathode = 0) then
         Port1_on := Port1_on or Set_DP_Mask; 
      end if;

      if not ( -- Long list of reasons NOT to display a digit!
         ( not TwentyFour and then (Cathode = 3) and then (TheDigit = 0)) or else
         -- Blank digit 3 (leading zero) in 12 hour mode
         ( Osc_Fault and then Ticks >= Seven_Eighths) or else       
         -- Indicate oscillator failure by occulting all digits for last 1/8 second
         -- User is thereby encouraged to check the time and start the setting process to 
         -- clear this error!
         (Ticks >= Half_Second and then
         -- between 0.5 and 1 second ... flash part or all of display 
         -- while setting, to indicate setting mode
            ((SetMode = set_hour   and then Cathode >= 2) or else
             (SetMode = set_minute and then Cathode <  2) or else
              SetMode = set_second or else
              SetMode = set_24))) 
      then
         CPU.port1_r.p1out := Port1_on;
      end if;
         
      -- to reduce brightness we could busywait here then turn cathodes off...

      -- next digit
      Cathode := (Cathode + 1) mod 4;

      -- handle button state
      if CPU.port1_r.p1in_bits(7) then	-- not pressed
         Pulse_Time := Timer.tar - Button_Time;
         if Pressed then -- button has just been released
            Pressed := FALSE;
            if Pulse_Time > Glitch and Pulse_Time < Second then
               if SetMode = set_24 then 
                  TwentyFour := not TwentyFour;
               elsif SetMode = set_hour then
                  Shared.Hour;
               elsif SetMode = set_minute then
                  Shared.Minute;
                  -- reset to the start of the current minute
                  -- so that minutes don't roll forward a minute before we can set seconds
                  Timer.tar  := 0;
                  Timer.ccr1 := 2; -- update display timer or we lose the scanning!
               elsif SetMode = set_second then -- set seconds to 0
                  SetMode    := normal;
                  Timer.tar  := 0;
                  Timer.ccr1 := 2; -- update display timer or we lose the scanning!
               end if;
            end if;
         end if;
      else                              -- pressed
         if not Pressed then   -- button press has just started
            Pressed := TRUE;
            Display_Ticks := Timeout;	-- reset timeout to 10s
            Button_Time := Timer.tar;	-- log time
            Acted := False;
         elsif not Acted and then Timer.tar - Button_Time >= Second then
            -- we are in the setting cycle, so clear the oscillator fault flag
            Osc_Fault := False;
            SetMode := SetState'Succ(SetMode);
            if SetMode = last then SetMode := normal; end if;
            -- ensure we wait a whole minute before timeout leaves Set_Seconds
            if SetMode = set_second then Display_Ticks := Timeout_Minute; end if;
            Acted := True;
         end if;
      end if;

      Enter_LPM3;
      -- Either display timer or button press released us from LPM3
   end loop;

   -- turn display off again before we sleep
   display_off;
end Button_Pressed;

-- Main program
begin
    -- Initialise registers
   Setup_Timer;
   Setup_Ports;
   Setup_CPU;	
   loop
      Enter_LPM3;
      -- as the display interrupt is not running, only a button press can wake up.
      -- button press calls "Button_Pressed", returning to the loop (and sleep) when done
      Button_Pressed;
   end loop;
end Watch; 

