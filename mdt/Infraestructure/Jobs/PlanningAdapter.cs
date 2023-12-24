using System;
using System.Net.Http;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs.DTO;
using Newtonsoft.Json;

namespace DDDSample1.Infrastructure.Jobs
{
    public class PlanningAdapter
    {
        private string _url = "http://localhost:8090/api/";

        private HttpClient _client;

        public PlanningAdapter(HttpClient httpClient)
        {
            _client = httpClient;
        }

        public async Task<TaskSequenceDto> ComputeSequence(ComputeSequenceDto dto)
        {
            string url = _url + "task-sequence";

            string content = JsonConvert.SerializeObject(dto);
            Console.WriteLine(content);

            var stringContent = new StringContent(
                content,
                System.Text.Encoding.UTF8,
                "application/json"
            );

            var response = await _client.PostAsync(url, stringContent);

            if (response.IsSuccessStatusCode)
            {
                var responseContent = await response.Content.ReadAsStringAsync();
                return JsonConvert.DeserializeObject<TaskSequenceDto>(responseContent);
            }

            throw new HttpRequestException("Error in the request to the planning service.");
        }
    }
}
