with Interfaces;
package Shared is

procedure Minute;

procedure Hour;

Hours   : Interfaces.Unsigned_8 := 0;
pragma Volatile(Hours);

Minutes : Interfaces.Unsigned_8 := 0;
pragma Volatile(Minutes);

end Shared;
