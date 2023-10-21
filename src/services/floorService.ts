import { Service, Inject } from 'typedi'
import config from '../../config'
import { IFloorDTO } from '../dto/IFloorDTO'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'
import IFloorService from './IServices/IFloorService'
import { Result } from '../core/logic/Result'
import { Floor } from '../domain/floor/floor'
import { FloorMap } from '../mappers/FloorMap'
import { BuildingId } from '../domain/building/buildingId'
import { BuildingCode } from '../domain/building/buildingCode'
import { FloorNumber } from '../domain/floor/floorNumber'
import { Description } from '../domain/description'
import { floor } from 'lodash'

@Service()
export default class FloorService implements IFloorService {
    constructor(@Inject(config.repos.floor.name) private floorRepo: IFloorRepo,@Inject(config.repos.building.name) private buildingRepo: IBuildingRepo) {}

    public async createFloor(floorDTO: IFloorDTO, buildingCode: string): Promise<Result<IFloorDTO>> {
        try {
            const building = await this.buildingRepo.findByCode(BuildingCode.create(buildingCode).getValue())
            if(building===null){
                return Result.fail<IFloorDTO>("Building not found");
            }

            floorDTO.buildingCode = buildingCode;
            const floorOrError = await Floor.create({
                building: building,
                floorNumber: FloorNumber.create(floorDTO.floorNumber).getValue(),
                description: Description.create(floorDTO.description).getValue()
            })
            if (floorOrError.error) {
                return Result.fail<IFloorDTO>(("Floor not created"));
            }

            if(await this.floorRepo.exists(floorOrError.getValue())){
                console.log("Floor already exists")
            }else{
                console.log("Floor does not exist")
            }

            await this.floorRepo.save(floorOrError.getValue())

            const floorDTOResult = FloorMap.toDTO(floorOrError.getValue()) as IFloorDTO
            return Result.ok<IFloorDTO>(floorDTOResult)
        } catch (e) {
            throw e
        }
    }
}
