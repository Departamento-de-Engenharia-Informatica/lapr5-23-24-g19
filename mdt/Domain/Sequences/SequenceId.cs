using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.Sequences
{
    public class SequenceId : EntityId
    {
        [JsonConstructor]
        public SequenceId(Guid value)
            : base(value) { }

        public SequenceId(String value)
            : base(value) { }

        protected override Object createFromString(String text)
        {
            return new Guid(text);
        }

        public override String AsString()
        {
            Guid obj = (Guid)base.ObjValue;
            return obj.ToString();
        }

        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }
    }
}
