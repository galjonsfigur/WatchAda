with Interfaces; 
use Interfaces;

package TickCounts is

subtype TickCount is Interfaces.Unsigned_16;

OneMinute      : constant TickCount := 1024*60;	
Second         : constant TickCount := 1024;
SecondMask     : constant TickCount := Second - 1;
Half_Second    : constant TickCount := 512;
Seven_Eighths  : constant TickCount := 512 + 256 + 128;
Display_Time   : constant TickCount := 10; 	-- seconds

-- we have to bump timer compare by 2 ticks to get a timer interrupt
-- This limits the scanning rate to 512Hz at ACLK/4=1024Hz 
-- Reflected here in Seconds/2
Timeout        : constant TickCount := Second/2 * Display_Time;
Timeout_Minute : constant TickCount := Second/2 * 60;

-- Debounce switch by ignoring presses shorter than this
Glitch         : constant TickCount := 25; 

end TickCounts;
