using System.Text.Json.Serialization;

namespace DDDSample1.Domain.Jobs
{
    public class UpdatingJobDto
    {
        [JsonPropertyName("jobStatus")]
        public string JobStatus { get; set; }

        public string JobId { get; set; }
    }
}
