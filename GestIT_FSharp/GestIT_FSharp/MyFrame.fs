/// <summary>
/// MyFrame is a personal representation of LEAP's frame informations.
/// </summary>
namespace MyLeapFrame

    open Leap
    open System.Collections.Generic

    /// <summary>
    /// Representation of ids.
    /// </summary>
    [<AllowNullLiteralAttribute>]
    type FakeId () =
        class
        end

    /// typeparam name="TimeStamp">
    /// Defines a generic type of timestamp.
    /// </typeparam>
    type TimeStamp = int64

    /// <summary>
    /// Defines a Pointable object, which could be a finger or a tool.
    /// </summary>
    /// <param name="i">FakeId.</param>
    /// <param name="ih">FakeId of the hand to which the Pointable belongs to.</param>
    /// <param name="d">A Vector representing direction.</param>
    /// <param name="p">A Vector representing position.</param>
    /// <param name="v">A Vector representing velocity.</param>
    /// <param name="finger">A boolean representing if Pointable is a finger.</param>
    /// <param name="tool">A boolean representing if Pointable is a tool.</param>
    /// <param name="l">Length of Pointable.</param>
    /// <param name="w">Width of Pointable.</param>
    type MyPointable (i:FakeId, ih:FakeId, d:Vector, p:Vector, v:Vector, finger:bool, tool:bool, l:float32, w:float32) =
        let mutable direction = new Vector(d)
        let mutable position = new Vector(p)
        let mutable velocity = new Vector(v)
        let id = i
        let mutable idHand = ih
        let isFinger = finger
        let isTool = tool
        let length = l
        let width = w
        member this.Direction with get() = direction and set(d) = direction <- new Vector(d)
        member this.Position with get() = position and set(p) = position <- new Vector(p)
        member this.Velocity with get() = velocity and set(v) = velocity <- new Vector(v)
        member this.Id with get() = id
        member this.IdHand with get() = idHand and set(v) = idHand <- v
        member this.Length = length
        member this.Width = width
        member this.IsFinger = isFinger
        member this.IsTool = isTool
        member this.Clone () =
            new MyPointable(this.Id, this.IdHand, this.Direction, this.Position, this.Velocity, this.IsFinger, this.IsTool, this.Length, this.Width)
        //member this.Rename (newi,newih) = new MyPointable(newi,newih,direction,position,velocity,isFinger,isTool,length,width)
        
    type MyHand (i:FakeId, d:Vector, p:Vector, v:Vector, n:Vector, c:Vector, r:float32) =
        let mutable direction = new Vector(d)
        let mutable position = new Vector(p)
        let mutable velocity = new Vector(v)
        let normal = new Vector(n)
        let sphereCenter = new Vector(c)
        let sphereRadius = r
        let id = i
        member this.Direction = direction
        member this.Position with get() = position and set(p) = position <- p
        member this.Velocity = velocity
        member this.Normal = normal
        member this.Id with get() = id
        member this.SphereCenter = sphereCenter
        member this.SphereRadius = sphereRadius
        member this.Clone () =
            new MyHand(this.Id, this.Direction, this.Position, this.Velocity, this.Normal, this.SphereCenter, this.SphereRadius)
        //member this.Rename (newi) = new MyHand(newi,direction,position,velocity,normal,sphereCenter,sphereRadius)

    type MyFrame () =
        //let mutable idFrame = f.Id
        let pointableList = new Dictionary<FakeId,MyPointable>()
        let handList = new Dictionary<FakeId,MyHand>()
        let mutable timestamp:TimeStamp = -1L
        (*do
            if f.Equals(Frame.Invalid) then
                idFrame <- (int64)(-1)
            else 
                for h in f.Hands do
                    let hh = new MyHand(h)
                    handList.Add(hh.Id, hh)
                for p in f.Pointables do
                    let pp = new MyPointable(p)
                    pointableList.Add(pp.Id, pp)
                    *)
        //member this.IdFrame = idFrame
        member this.PointableList = pointableList
        member this.HandList = handList
        member this.Timestamp with get() = timestamp and set(t) = timestamp <- t
        member this.IsValid = timestamp >= 0L
        member this.Clone () =
            let f = new MyFrame()
            f.Timestamp <- this.Timestamp
            for h in this.HandList do
                f.HandList.Add(h.Key, h.Value.Clone())
            for p in this.PointableList do
                f.PointableList.Add(p.Key, p.Value.Clone())
            f

        (*
            if idFrame <> (int64)(-1) then true
            else false
            *)