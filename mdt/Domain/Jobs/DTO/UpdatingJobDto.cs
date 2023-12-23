using System.Text.Json.Serialization;

namespace DDDSample1.Domain.Jobs
{
    public class UpdatingJobDto
    {
        // FIXME: JsonPropertyName not working
        [JsonPropertyName("taskStatus")]
        public string JobStatus { get; set; }

        public string JobId { get; set; }
    }
}
