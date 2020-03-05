with CPU;
with System.GCC_Builtins;
with Interfaces;
use type Interfaces.Unsigned_8;
use type Interfaces.Unsigned_16;
with TickCounts;

package body Shared is

   package Timer renames CPU.ta3;

   Carry : Boolean := False;

procedure Hour is
begin
   Hours := Hours + 1;
   if Hours = 24 then
      Hours := 0;
   end if;
end Hour;

procedure Minute is
begin 
   Minutes := Minutes  + 1;
   if Minutes = 60 then
      Minutes := 0;
      if Carry then
         Hour;
      end if;
   end if;
end Minute;


procedure Minute_Interrupt;
pragma Machine_Attribute (Entity => Minute_Interrupt,
                          Attribute_Name => "interrupt",
                          Info => CPU.vectors.timer0_a0_vector);

procedure Minute_Interrupt is
begin
   Carry := True;
   Minute;
   Carry := False;
   -- Minute interrupt does not interfere with power down state.
   -- It should probably not call other procedures; the official 
   -- TI (not gcc) C compiler manual(slau132)  suggested that is 
   -- bad form and leads to unsaved registers for the called procedures.
   -- However, (while Minute is unshared) they are simply inlined.
   -- Now Minute is shared, a (pessimistic looking) set of registers is preserved.
end Minute_Interrupt;

procedure Display_Interrupt;
pragma Machine_Attribute (Entity => Display_Interrupt,
                          Attribute_Name => "interrupt",
                          Info => CPU.vectors.timer0_a1_vector);

procedure Display_Interrupt is
Temp : Interfaces.Unsigned_16 := Timer.taiv;  -- read taiv to clear interrupt
begin
   Temp    := Timer.tar + 2;	   -- do this first to guarantee synch
   if Temp >= TickCounts.OneMinute then 
      Temp := 1;
   end if;
   Timer.ccr1    := Temp;	   -- book the next interrupt!
   -- and leave sleep mode to return to display loop
   System.GCC_Builtins.bic_status_register_on_exit(CPU.cpu.lpm3);
end Display_Interrupt;

procedure Button_Interrupt;
pragma Machine_Attribute (Entity => Button_Interrupt,
                          Attribute_Name => "interrupt",
                          Info => CPU.vectors.port1_vector);

procedure Button_Interrupt is
begin  
    -- clear button press interrupt and exit LPM3.
    CPU.port1_r.p1ifg  := 0;
    System.GCC_Builtins.bic_status_register_on_exit(CPU.cpu.lpm3);
end Button_Interrupt;

end Shared;
