using System.Threading.Tasks;
using DDDSample1.Domain.Jobs.DTO;

namespace DDDNetCore.Infraestructure.Jobs;

public interface IPlanningAdapter
{
    Task<TaskSequenceDto> ComputeSequence(ComputeSequenceDto dto);
    Task<string[]> GetSequenceAlgorithms();
}
