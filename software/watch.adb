with Interfaces; use Interfaces;
with CPU;
with System.GCC_Builtins;
with TickCounts; use TickCounts;

with Shared;

procedure Watch is

   package Timer renames CPU.ta3;
   package Clock renames CPU.bc2;
   package ADC renames CPU.adc10;

subtype Digit is Interfaces.Unsigned_8 range 0 .. 9;
subtype cathodes is Interfaces.Unsigned_8 range 0 .. 3;
subtype Segments is unsigned_8;
type ADC_Channel is (light_sensor, temperature_sensor, battery_low_range, battery_high_range);
-- name significant port or register bits
Osc_Fault : Boolean renames CPU.cpu.ifg1_bits(1);

-- port bit assignments
-- P2 : 0 => a, 1=> b, 2 => c, 3 => d, 4 => e, 5 => f 
--      (6=> xtal in, 7 => xtal out)
-- P1 : 0 => an1, 1 => an2, 2 => an3, 3 => an4
--      4 => g, 5 => dp, 6 => light sensor, 7 -> switch

-- port state with all cathodes turned off
Port1_off    : constant Unsigned_8  := 16#B0#;

Set_DP_Mask  : constant Unsigned_8  := 16#20#;
-- (bit 5 on, all others off since this is a mask)

-- Lookup1 contains the port bit allocations to display each digit on port 1
-- Lookup2 contains the port bit allocations to display each digit on port 2
Lookup2 : constant array (Digit) of Segments :=
    (0 => 2#00111111#,	-- 0 = off, 1 = on
     1 => 2#00000110#,
     2 => 2#00011011#,
     3 => 2#00001111#, 
     4 => 2#00100110#,
     5 => 2#00101101#,
     6 => 2#00111101#,
     7 => 2#00000111#,
     8 => 2#00111111#,
     9 => 2#00101111#);  

Lookup1 : constant array (Digit) of Segments :=
    (0 => 2#10000000#,  -- 0 = off, 1 = on, switch pin always on
     1 => 2#10000000#,
     2 => 2#10010000#,
     3 => 2#10010000#,
     4 => 2#10010000#,
     5 => 2#10010000#,
     6 => 2#10010000#,
     7 => 2#10000000#,
     8 => 2#10010000#,
     9 => 2#10010000#);

-- translate cathode number to the port bit value that drives it
-- to drive common cathode displays via low value resistor
Select_Cathode : constant array(Cathodes) of Unsigned_8 := (8,4,2,1);

-- Lookup24_1 and Lookup24_2 will display "12:__" when TwentyFour is false,
-- or "24:__" when TwentyFour is true, for setting 12/24 hour mode.
Lookup24_2 : constant array (Boolean,Cathodes) of Segments :=
   (False => (3 => Lookup2(1),
              2 => Lookup2(2),
              1 => Lookup2(0), -- Never accesed
              0 => Lookup2(0)), -- Never accesed
    True  => (3 => Lookup2(2),
              2 => Lookup2(4), 
              1 => Lookup2(0), -- Never accesed
              0 => Lookup2(0))); -- Never accesed

Lookup24_1 : constant array (Boolean,Cathodes) of Segments :=
   (False => (3 => Lookup1(1) xor Select_Cathode(3), 
              2 => Lookup1(2) xor Select_Cathode(2), 
              1 => Port1_off, 
              0 => Port1_off),
    True  => (3 => Lookup1(2) xor Select_Cathode(3), 
              2 => Lookup1(4) xor Select_Cathode(2), 
              1 => Port1_off, 
              0 => Port1_off));

-- LookupCelcius_1 and LookupCelcius_2 will display `*C` on display for temperature display
LookupCelcius_1 : constant array (Cathodes) of Segments :=
   (0 => 2#10000000# xor Select_Cathode(0), -- 0 = off, 1 = on, switch pin always on
    1 => 2#10010000# xor Select_Cathode(1),
    2 => Port1_off,
    3 => Port1_off);

LookupCelcius_2 : constant array (Cathodes) of Segments :=
    (0 => 2#00111001#,  -- 0 = off, 1 = on
     1 => 2#00100011#,
     2 => Lookup2(0),
     3 => Lookup2(0));  

-- LookupEgg_1 and LookupEgg_2 for easter egg display
LookupEgg_1 : constant array (Cathodes) of Segments :=
   (0 => 2#10100000# xor Select_Cathode(0), -- 0 = off, 1 = on, switch pin always on
    1 => 2#10010000# xor Select_Cathode(1),
    2 => 2#10010000# xor Select_Cathode(2),
    3 => 2#10010000# xor Select_Cathode(3));
LookupEgg_2 : constant array (Cathodes) of Segments :=
    (0 => 2#00000110#,  -- 0 = off, 1 = on
     1 => 2#00110111#,
     2 => 2#00011110#,
     3 => 2#00110111#);

-- LookupVoltage_1 and LookupVoltage_2 for showing battery voltage
LookupVoltage_1 : constant array (Cathodes) of Segments :=
   (0 => 2#10000000# xor Select_Cathode(0), -- 0 = off, 1 = on, switch pin always on
    1 => Port1_off,
    2 => Port1_off,
    3 => 2#10100000# xor Select_Cathode(3));

LookupVoltage_2 : constant array (Cathodes) of Segments :=
    (0 => 2#00011100#,  -- 0 = off, 1 = on
     1 => Lookup2(0),
     2 => Lookup2(0),
     3 => Lookup2(0));  

-- Local variables for main subprogram
TwentyFour : Boolean := True;
PM         : Boolean := False;

procedure Setup_Timer is
begin
   -- basic clock setup
   Clock.dcoctl  := 0; -- lowest DCOCTL frequency
   -- ClkCtrl 1	Calibration	-- Aclk = XT1 LF (32768) / 8 = 4096, XT2 off
   Clock.bcsctl1 := CPU.calibration.calbc1_1mhz or Clock.xt2off or Clock.diva_3;
   Clock.dcoctl  := CPU.calibration.caldco_1mhz;
   Clock.bcsctl2 := Clock.selm_0 or Clock.divm_0; -- Mclk = DCO = 1MHz
   Clock.bcsctl3 := Clock.xcap_3; -- 32kHz osc, 12.5 pf loading
   -- timer A3 setup
   Timer.ccr0    := OneMinute - 1; -- 1024*60;	 ticks per minute
   Timer.tactl   := Timer.tassel_1 or Timer.id_2 or Timer.mc_1 or Timer.taclr; -- select ACLK/4 = 1024Hz, up, clear
   Timer.cctl0   := Timer.ccie; -- enable capture/control 0 interrupt
   Timer.cctl1   := 0; -- capture/control 1 used for display scanning
   Timer.cctl2   := 0; -- capture/control 2 unused here
end Setup_Timer;

procedure Setup_Ports is
use CPU.cpu, CPU.port1_r;
begin
    p2dir_bits := (6 => false, others => true);
    p2sel  := unsigned_8(bit6 or bit7);
    p2out_bits  := (others => false);
    p1dir  := unsigned_8(255 and not (bit6 or bit7));
    p1sel  := unsigned_8(bit6);
    p1out  := Port1_off;
--  P1 bit 7 is the switch input
    p1ren  := unsigned_8(bit7);	-- pullup on bit 7
--  Each PxIFG flag must be reset with software.
    p1ifg  := 0;
    p1ies  := unsigned_8(bit7);		-- falling edge
    p1ie   := unsigned_8(bit7);		-- and enable
end Setup_Ports;

procedure Enable_ADC( Channel : in ADC_Channel) is
use ADC;
begin
   -- set ADC clock to ~1MHz
   adc10ctl1 := ADC10SSEL_0 or adc10div_4;
   case Channel is
      when light_sensor =>
         -- set and enable 2.5V reference, turn ADC on, sample + hold x64 ADC10CLK, sample rate below 50 ksps, single measurement
         adc10ctl0 := sref_1 or ref2_5v or refon or adc10on or adc10sht_3 or adc10sr or conseq_0;
         -- set channel to P1.6
         adc10ctl1 := adc10ctl1 or inch_6;
         -- disable input/output buffer on P1.6
         adc10ae0_bits := (6 => true, others => false);
      when temperature_sensor =>
         -- set and enable 1.5V reference, turn ADC on, sample + hold x64 ADC10CLK, sample rate below 50 ksps, single measurement
         adc10ctl0 := sref_1 or refon or adc10on or adc10sht_3 or adc10sr or conseq_0;
         -- set channel to internal temperature sensor 
         adc10ctl1 := adc10ctl1 or inch_10;
      when battery_low_range =>
         -- set and enable 1.5V reference, turn ADC on, sample + hold x64 ADC10CLK, sample rate below 50 ksps, single measurement
         adc10ctl0 := sref_1 or refon or adc10on or adc10sht_3 or adc10sr or conseq_0;
         -- set channel to internal voltage divider 
         adc10ctl1 := adc10ctl1 or inch_11;
      when battery_high_range =>
         -- set and enable 2.5V reference, turn ADC on, sample + hold x64 ADC10CLK, sample rate below 50 ksps, single measurement
         adc10ctl0 := sref_1 or ref2_5v or refon or adc10on or adc10sht_3 or adc10sr or conseq_0;
         -- set channel to internal voltage divider 
         adc10ctl1 := adc10ctl1 or inch_11;
   end case;
   -- wait for reference voltage to settle
   for X in 0..128 loop
      System.GCC_Builtins.delay_cycles(1);
   end loop;
end Enable_ADC;

procedure Run_ADC is
begin
   ADC.adc10ctl0 := ADC.adc10ctl0 or ADC.enc or ADC.adc10sc;
   while (ADC.adc10ctl1 and ADC.adc10busy) /= 0 loop
      null; -- wait for conversion
   end loop;
end Run_ADC;

procedure Disable_ADC is
begin
   -- disable ADC and voltage reference (should be done automatically but just to be sure)
   ADC.adc10ctl0 := ADC.adc10ctl0 and not ADC.enc;
   ADC.adc10ctl0 := ADC.adc10ctl0 and not (ADC.adc10on or ADC.refon);
end Disable_ADC;

procedure Setup_CPU is
begin
    -- disable watchdog
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

procedure Display_On is
Temp : Unsigned_16 := Timer.tar + 2;
begin
   if Temp >= OneMinute then 
      Temp := 1;
   end if;
   Timer.ccr1    := Temp;
   Timer.cctl1   := Timer.ccie; -- enable display interrupt
end Display_On;

procedure Display_Off is
begin
   CPU.port1_r.p1out := Port1_off;
   Timer.cctl1   := 0; -- disable display interrupt
end Display_Off;

-- divide by 10 and remainder, avoiding expensive math library
procedure DivRem10(Units : in out Unsigned_8; Tens : out Unsigned_8 ) is
begin
   Tens := 0;
   while Units >= 10 loop
      Units := Units - 10;
      Tens := Tens + 1;
   end loop;
end DivRem10;

-- divide by 100 and remainder for voltage first digit
procedure DivRem100(Input : in out Unsigned_32; Hundreds : out Unsigned_8 ) is
begin
   Hundreds := 0;
   while Input >= 100 loop
      Input := Input - 100;
      Hundreds := Hundreds + 1;
   end loop;
end DivRem100;

-- states for the time setting state machine
type SetState is (normal, thermometer, voltmeter, set_24, set_hour, set_minute, set_second, egg, last);

procedure Button_Pressed is
Ticks         : TickCount;
Button_Time   : TickCount := Timer.tar;
Pulse_Time    : TickCount;
Pressed       : Boolean := True;
Acted         : Boolean := False;
Active        : Boolean := False;
SetMode       : SetState := normal;
Port1_on      : Unsigned_8;
HrMin         : Unsigned_8;
Tens          : Unsigned_8;
Hundreds      : Unsigned_8;
TheDigit      : Digit;
Display_Ticks : TickCount := Timeout;
ADC_Raw       : Unsigned_16;
Temperature   : Unsigned_32;
Voltage       : Unsigned_32;
currentDelay  : Unsigned_8 := 1;
targetDelay   : Unsigned_8 := 1; 
Cathode       : cathodes := 0;
begin
   -- read and process data from tempearture sensor before display loop
   -- enable ADC and start conversion
   Enable_ADC(temperature_sensor);
   Run_ADC;
   ADC_Raw := ADC.adc10mem; -- read the result
   -- Convert using magic formula
   Temperature := Shift_Right(Unsigned_32(ADC_Raw) * 27069 - 18169625, 16);
   -- Switch to battery measurement
   Disable_ADC;
   Enable_ADC(battery_low_range);
   Run_ADC;
   ADC_Raw := ADC.adc10mem; -- read the result
   if ADC_Raw = 16#3FF# then -- voltage above 3V, change reference
       -- Switch to high range
      Disable_ADC;  
      Enable_ADC(battery_high_range);
      Run_ADC;
      ADC_Raw := ADC.adc10mem; -- read the result  
      -- multiply by 500 and divide by 1024 to get 10s of mV
      Voltage := Shift_Right(Unsigned_32(ADC_Raw) * 500, 10);
   else
      -- multiply by 300 and divide by 1024 to get 10s of mV
      Voltage := Shift_Right(Unsigned_32(ADC_Raw) * 300, 10);
   end if;
   -- Switch again to light sensor and do another measurement
   Disable_ADC; 
   Enable_ADC(light_sensor);
   Run_ADC;   
   Display_On;
   DivRem100(Voltage, Hundreds);
   -- display scanning loop. Each iteration displays a digit for 2 ms; 
   -- the digit to display is indicated by Cathode (range 0 .. 3)
   while Display_Ticks > 0 loop
      Ticks         := Timer.tar and SecondMask;
      Display_Ticks := Display_Ticks - 1;
      -- read the result from next conversions
      while (ADC.adc10ctl1 and ADC.adc10busy) /= 0 loop
         null;
      end loop;
      -- read sample and set target delay
      ADC_Raw := ADC.adc10mem;
      -- values here are empirically chosen
      if ADC_Raw > 800 then
         targetDelay := 200;
      elsif ADC_Raw > 600 then
         targetDelay := 150;
      elsif ADC_Raw > 400 then
         targetDelay := 100;
      elsif ADC_Raw > 200 then
         targetDelay := 50;
      else
         targetDelay := 15;
      end if;

      -- calculate delay time to dim the display, change value on last digit
      -- to avoid segments with different brightness
      if currentDelay < targetDelay and Cathode = 3 then
         currentDelay := currentDelay + 1;
      elsif currentDelay > targetDelay and Cathode = 3 then
         currentDelay := currentDelay - 1;
      end if;
      -- load and process hour count. This is unconditional because we
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

      -- cathodes 0,1 control the minute digits, so display minutes instead.
      if Cathode < 2 then
         HrMin := Shared.Minutes;
      end if;
      if SetMode = thermometer then
         -- in thermometer mode: display temperature
         HrMin := Unsigned_8(Temperature);
      elsif SetMode = voltmeter then
         -- in voltmeter mode: diplay battery voltage
         HrMin := Unsigned_8(Voltage);
      end if;

      -- split number (hour or minute or temperature) into tens and units
      DivRem10(HrMin,Tens);
      -- select the correct digit to display, in voltmeter set correct order
      if SetMode /= voltmeter then
         case Cathode is
            when 0|2 => TheDigit := Digit(HrMin);
            when 1|3 => TheDigit := Digit(Tens);
         end case;
      else
         case Cathode is
            when 0|1 => TheDigit := Digit(HrMin); -- cathode - is overrided anyway 
            when 2   => TheDigit := Digit(Tens);
            when 3   => TheDigit := Digit(Hundreds);
         end case;
      end if;

      -- turn anodes off first to prevent visible glitches
      CPU.port1_r.p1out := Port1_off;

      -- write Port 2 directly.
      -- port 1 controls flashing, DP and other features
      -- so load a variable and modify it before writing to port.
      if SetMode = set_24 then -- display 12 or 24 (flashing, below)
         CPU.port1_r.p2out := Lookup24_2(TwentyFour,Cathode); 
         Port1_on := Lookup24_1(TwentyFour,Cathode);  
      elsif SetMode = thermometer and Cathode < 2 then
         CPU.port1_r.p2out := LookupCelcius_2(Cathode);
         Port1_on := LookupCelcius_1(Cathode);
      elsif SetMode = voltmeter and Cathode = 0 then
         CPU.port1_r.p2out := LookupVoltage_2(Cathode);
         Port1_on := LookupVoltage_1(Cathode);
      elsif SetMode = egg then
         CPU.port1_r.p2out := LookupEgg_2(Cathode);
         Port1_on := LookupEgg_1(Cathode);
      else
         CPU.port1_r.p2out := Lookup2(TheDigit); 
         Port1_on := Lookup1(TheDigit) xor Select_Cathode(Cathode); 
      end if;

      -- flash seconds indicator (currently DP of digit 2) 
      -- set PM indicator (currently DP of digit 0)
      -- set decimal separator in voltmeter
      if (Ticks < Half_Second and then Cathode = 2 and then SetMode /= voltmeter) or else (PM and then Cathode = 0) 
         or else (SetMode = voltmeter and then Cathode = 3) then
         Port1_on := Port1_on or Set_DP_Mask; 
      end if;

      if not ( -- long list of reasons NOT to display a digit!
         ( not TwentyFour and then (Cathode = 3) and then (TheDigit = 0) and then (SetMode /= set_24) and then (SetMode /= egg)) or else
         -- blank digit 3 (leading zero) in 12 hour mode
         ( Osc_Fault and then Ticks >= Seven_Eighths) or else       
         -- indicate oscillator failure by occulting all digits for last 1/8 second
         -- user is thereby encouraged to check the time and start the setting process to 
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
         
      -- to reduce brightness busywait and then turn cathodes off
      for X in 0..(currentDelay) loop
          System.GCC_Builtins.delay_cycles(1);
      end loop;
      CPU.port1_r.p1out := Port1_off;
      -- start nev conversion after disabling display
      ADC.adc10ctl0 := ADC.adc10ctl0 or ADC.enc or ADC.adc10sc;

      -- next digit
      Cathode := (Cathode + 1) mod 4;

      -- handle button state
      if CPU.port1_r.p1in_bits(7) then	-- not pressed
         Pulse_Time := Timer.tar - Button_Time;
         if Pressed then -- button has just been released
            Pressed := FALSE;
            if Pulse_Time > Glitch and Pulse_Time < Second then
               if SetMode = normal and Active then
                  SetMode := thermometer;
               elsif SetMode = thermometer then
                  SetMode := voltmeter;
               elsif SetMode = voltmeter or SetMode = egg then
                  SetMode := normal;
               elsif SetMode = set_24 then 
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
            -- assume that device is active after button press
            Active := True;
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
            -- skip egg if long press in set_second mode
            if SetMode = egg then SetMode := SetState'Succ(SetMode); end if;
            -- enter egg if long press in voltmeter
            if SetMode = set_24 then SetMode := egg; end if;
            -- skip thermometer
            if SetMode = thermometer then SetMode := SetState'Succ(SetMode); end if;
            -- also skip voltmeter
            if SetMode = voltmeter then SetMode := SetState'Succ(SetMode); end if;
            -- roll over state if needed
            if SetMode = last then SetMode := normal; end if;
            -- ensure we wait a whole minute before timeout leaves Set_Seconds
            if SetMode = set_second then Display_Ticks := Timeout_Minute; end if;
            Acted := True;
         end if;
      end if;

      Enter_LPM3;
      -- either display timer or button press will release us from LPM3
   end loop;

   -- turn display off and shutdown ADC again before we sleep
   Active := false;
   Display_Off;
   Disable_ADC;
   
end Button_Pressed;

-- main program
begin
    -- initialise registers
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

