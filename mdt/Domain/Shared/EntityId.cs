using System;

namespace DDDSample1.Domain.Shared
{
    /// <summary>
    /// Base class for entities.
    /// </summary>
    public abstract class EntityId: IEquatable<EntityId>, IComparable<EntityId>
    {
        protected Object ObjValue {get;}

        public String Value { 
            get { 
                    if (this.ObjValue.GetType() == typeof(String))
                        return (String) this.ObjValue;
                    return AsString();
                } 
        }

        protected EntityId(Object value)
        {
            if (value.GetType() == typeof(String))
                this.ObjValue = createFromString((String)value);
            else
                this.ObjValue = value;
        }

       
        protected abstract Object createFromString(String text);
        
        public abstract String AsString();


        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is EntityId other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public bool Equals(EntityId other)
        {
            if (other == null)
                return false;
            if (this.GetType() != other.GetType())
                return false;
            return this.Value == other.Value;
        }

        public int CompareTo(EntityId other){
            if (other == null)
                return -1;
            return this.Value.CompareTo(other.Value);
        }

        public static bool operator ==(EntityId obj1, EntityId obj2)
        {
            if (object.Equals(obj1, null))
            {
                if (object.Equals(obj2, null))
                {
                    return true;
                }
                return false;
            }
            return obj1.Equals(obj2);
        }
        public static bool operator !=(EntityId x, EntityId y) 
        {
            return !(x == y);
        }
    }
   
}