import { Service, Inject } from 'typedi'
import config from '../../config'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'
import { Result } from '../core/logic/Result'
import { BuildingCode } from '../domain/building/buildingCode'
import { FloorNumber } from '../domain/floor/floorNumber'
import { IPassageDTO } from '../dto/IPassageDTO'
import IPassageService from './IServices/IPassageService'
import IPassageRepo from './IRepos/IPassageRepo'
import { Passage } from '../domain/passage/passage'
import { PassageMap } from '../mappers/PassageMap'
import Building from '../domain/building/building'

@Service()
export default class PassageService implements IPassageService{
    constructor(
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.passage.name) private passageRepo: IPassageRepo,
        ) {}

    public async createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>> {
        try {
            //verify if building exist
            if(!this.existBuildings(passageDTO.floor1.buildingCode,passageDTO.floor2.buildingCode)){
                return Result.fail<IPassageDTO>("Building not found");
            }

            //verify if floors exist
            const floor1 = await this.floorRepo.findByCodeNumber(
                    BuildingCode.create(passageDTO.floor1.buildingCode).getValue(),
                    FloorNumber.create(passageDTO.floor1.floorNumber).getValue()
            )
            const floor2 = await this.floorRepo.findByCodeNumber(
                BuildingCode.create(passageDTO.floor2.buildingCode).getValue(),
                FloorNumber.create(passageDTO.floor2.floorNumber).getValue()
            )
            if(floor1===null || floor2===null){
                return Result.fail<IPassageDTO>("Floor not found");
            }

            const passageOrError = Passage.create({floor1,floor2})

            if (passageOrError.error) {
                return Result.fail<IPassageDTO>(passageOrError.error.toString());
            }
            
            if(await this.passageRepo.exists(passageOrError.getValue())){
                return Result.fail<IPassageDTO>("Passage already exists")
            }else{
                await this.passageRepo.save(passageOrError.getValue())
                const passageDTOResult= PassageMap.toDTO(passageOrError.getValue()) as IPassageDTO
                return Result.ok<IPassageDTO>(passageDTOResult)
            }
                
        } catch (e) {
            throw e
        }
    }

    private async existBuildings(building1Str: string, building2Str: string): Promise<boolean> {
        const building1 = await this.buildingRepo.findByCode(BuildingCode.create(building1Str).getValue())
        const building2 = await this.buildingRepo.findByCode(BuildingCode.create(building2Str).getValue())

        if(building1===null || building2===null){
            return false
        }
        return true
    }

}
