/// <summary>
/// MyFrame is a personal representation of LEAP's frame informations.
/// </summary>
namespace ClonableLeapFrame

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
    type ClonablePointable (i:FakeId, ih:FakeId, d:Vector, p:Vector, v:Vector, finger:bool, tool:bool, l:float32, w:float32) =
        let mutable direction = new Vector(d)
        let mutable position = new Vector(p)
        let mutable velocity = new Vector(v)
        let id = i
        let mutable idHand = ih
        let isFinger = finger
        let isTool = tool
        let length = l
        let width = w
        /// <summary>
        /// Get or set direction property.
        /// </summary>
        /// <param name="d">Direction.</param>
        member this.Direction with get() = direction and set(d) = direction <- new Vector(d)
        /// <summary>
        /// Get or set position property.
        /// </summary>
        /// <param name="d">Position.</param>
        member this.Position with get() = position and set(p) = position <- new Vector(p)
        /// <summary>
        /// Get or set velocity property.
        /// </summary>
        /// <param name="d">Velocity.</param>
        member this.Velocity with get() = velocity and set(v) = velocity <- new Vector(v)
        member this.Id with get() = id
        /// <summary>
        /// Get or set the id of the hand to which the pointable belongs to.
        /// </summary>
        /// <param name="d">Hand id.</param>
        member this.IdHand with get() = idHand and set(v) = idHand <- v
        member this.Length = length
        member this.Width = width
        member this.IsFinger = isFinger
        member this.IsTool = isTool
        /// <summary>
        /// Clones the MyPointable object on which the method is called.
        /// </summary>
        member this.Clone () =
            new ClonablePointable(this.Id, this.IdHand, this.Direction, this.Position, this.Velocity, this.IsFinger, this.IsTool, this.Length, this.Width)
      
    /// <summary>
    /// Defines a Hand object.
    /// </summary>
    /// <param name="i">FakeId.</param>
    /// <param name="d">A Vector representing direction.</param>
    /// <param name="p">A Vector representing position.</param>
    /// <param name="v">A Vector representing velocity.</param>
    /// <param name="n">A Vector perpendicular to the plane formed by the palm of the hand.</param>
    /// <param name="c">A Vector representing the center of the sphere.</param>
    /// <param name="r">The radius of the sphere.</param> 
    type ClonableHand (i:FakeId, d:Vector, p:Vector, v:Vector, n:Vector, c:Vector, r:float32) =
        let mutable direction = new Vector(d)
        let mutable position = new Vector(p)
        let mutable velocity = new Vector(v)
        let normal = new Vector(n)
        let sphereCenter = new Vector(c)
        let sphereRadius = r
        let id = i
        /// <summary>
        /// Get or set direction property.
        /// </summary>
        /// <param name="d">Direction.</param>
        member this.Direction with get() = direction and set(d) = direction <- d
        /// <summary>
        /// Get or set position property.
        /// </summary>
        /// <param name="d">Position.</param>
        member this.Position with get() = position and set(p) = position <- p
        /// <summary>
        /// Get or set velocity property.
        /// </summary>
        /// <param name="d">Velocity.</param>
        member this.Velocity with get() = velocity and set(v) = velocity <- v
        member this.Normal = normal
        /// <summary>
        /// Get the hand id.
        /// </summary>
        member this.Id with get() = id
        member this.SphereCenter = sphereCenter
        member this.SphereRadius = sphereRadius
        /// <summary>
        /// Clones the MyHand object on which the method is called.
        /// </summary>
        member this.Clone () =
            new ClonableHand(this.Id, this.Direction, this.Position, this.Velocity, this.Normal, this.SphereCenter, this.SphereRadius)

    /// <summary>
    /// Defines a Frame object. When created, a frame is always invalid.
    /// </summary>
    type ClonableFrame () =
        let pointableList = new Dictionary<FakeId,ClonablePointable>()
        let handList = new Dictionary<FakeId,ClonableHand>()
        let mutable timestamp:TimeStamp = -1L
        member this.PointableList = pointableList
        member this.HandList = handList
        member this.Timestamp with get() = timestamp and set(t) = timestamp <- t
        member this.IsValid = timestamp >= 0L
        /// <summary>
        /// Clones the MyFrame object on which the method is called.
        /// </summary>
        member this.Clone () =
            let f = new ClonableFrame()
            f.Timestamp <- this.Timestamp
            for h in this.HandList do
                f.HandList.Add(h.Key, h.Value.Clone())
            for p in this.PointableList do
                f.PointableList.Add(p.Key, p.Value.Clone())
            f
