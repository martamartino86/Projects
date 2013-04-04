module MyFrame

    open Leap
    open System.Collections.Generic

    type FakeId = obj
    type LeapId = int
    type TimeStamp = int64

    type MyPointable (i:FakeId, ih:FakeId, d:Vector, p:Vector, v:Vector, finger:bool, tool:bool, l:float32, w:float32) =
        let mutable direction = new Vector(d)
        let mutable position = new Vector(p)
        let mutable velocity = new Vector(v)
        let id = i
        let idHand = ih
        let isFinger = finger
        let isTool = tool
        let length = l
        let width = w
        //new(p:Pointable) = MyPointable(p.Id, p.Hand.Id, p.Direction, p.TipPosition, p.TipVelocity, p.IsFinger, p.IsTool, p.Length, p.Width)
        member this.Direction = direction
        member this.Position with get() = position and set(p) = position <- p
        member this.Velocity = velocity
        member this.Id with get() = id
        member this.IdHand with get() = idHand
        member this.Length = length
        member this.Width = width
        member this.IsFinger = isFinger
        member this.IsTool = isTool
        //member this.Rename (newi,newih) = new MyPointable(newi,newih,direction,position,velocity,isFinger,isTool,length,width)
        
    type MyHand (i:FakeId, d:Vector, p:Vector, v:Vector, n:Vector, c:Vector, r:float32) =
        let mutable direction = new Vector(d)
        let mutable position = new Vector(p)
        let mutable velocity = new Vector(v)
        let normal = new Vector(n)
        let sphereCenter = new Vector(c)
        let sphereRadius = r
        let id = i
        //new(h:Hand) = MyHand(h.Id, h.Direction, h.PalmPosition, h.PalmVelocity, h.PalmNormal, h.SphereCenter, h.SphereRadius)
        member this.Direction = direction
        member this.Position with get() = position and set(p) = position <- p
        member this.Velocity = velocity
        member this.Normal = normal
        member this.Id with get() = id
        member this.SphereCenter = sphereCenter
        member this.SphereRadius = sphereRadius
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
        (*
            if idFrame <> (int64)(-1) then true
            else false
            *)