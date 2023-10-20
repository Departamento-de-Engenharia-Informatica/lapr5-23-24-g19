/*import { Service, Inject } from 'typedi'
import config from '../../config'
import { IFloorDTO } from '../dto/IFloorDTO'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import IFloorService from './IServices/IFloorService'
import { Result } from '../core/logic/Result'

@Service()
export default class FloorService implements IFloorService {
    constructor(@Inject(config.repos.Floor.name) private FloorRepo: IFloorRepo) {}

    public async createFloor(FloorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
        try {
            const FloorOrError = Floor.create(FloorDTO)

            if (FloorOrError.isFailure) {
                return Result.fail<IFloorDTO>(FloorOrError.errorValue())
            }

            const FloorResult = FloorOrError.getValue()

            await this.FloorRepo.save(FloorResult)

            const FloorDTOResult = FloorMap.toDTO(FloorResult) as IFloorDTO
            return Result.ok<IFloorDTO>(FloorDTOResult)
        } catch (e) {
            throw e
        }
    }
}*/
