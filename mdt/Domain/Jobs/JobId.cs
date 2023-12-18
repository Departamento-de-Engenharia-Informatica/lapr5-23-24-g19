using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.Jobs
{
    public class JobId : EntityId
    {
        [JsonConstructor]
        public JobId(Guid value)
            : base(value) { }

        public JobId(String value)
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
