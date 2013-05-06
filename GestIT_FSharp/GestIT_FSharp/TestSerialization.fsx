#r @"C:\Users\Pc\Desktop\Leap_Developer_Kit_0.7.8_3421_Windows\LeapSDK\lib\LeapCSharp.NET3.5.dll"
#load "GestIT_Library.fs"
#load "ClonableFrame.fs"
#load "Driver.fs"

open System.IO
open System.Runtime.Serialization.Formatters.Binary

let outf = @"C:\Users\Pc\Desktop\output.ser"
let f = File.OpenWrite(outf)

let formatter = new BinaryFormatter()

let fakeid = ClonableLeapFrame.FakeId()
fakeid.ID

formatter.Serialize(f, (System.DateTime.Now, LeapDriver.LeapFeatureTypes.ActiveFinger, new LeapDriver.LeapEventArgs(new ClonableLeapFrame.ClonableFrame(), fakeid)))
formatter.Serialize(f, (System.DateTime.Now, LeapDriver.LeapFeatureTypes.ActiveHand, new LeapDriver.LeapEventArgs(new ClonableLeapFrame.ClonableFrame(), ClonableLeapFrame.FakeId())))

f.Flush()

f.Close()

let ff = File.OpenRead(outf)

let frm = new BinaryFormatter()

let d = frm.Deserialize(ff) :?> System.DateTime*LeapDriver.LeapFeatureTypes*LeapDriver.LeapEventArgs

ff.Length = ff.Position

ff.Close()

match d with
| a,b,c -> c.Id.ID.Equals(fakeid.ID)

match d with
| a,b,c -> c.Id.Equals(fakeid)

fakeid

net

net.Stream <- f

...

net.Stream <- null
