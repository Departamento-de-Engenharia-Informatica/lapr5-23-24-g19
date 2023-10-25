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

@Service()
export default class PassageService implements IPassageService {
    constructor(
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.passage.name) private passageRepo: IPassageRepo,
    ) {}

    public async createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>> {
        try {
            //verify if building exist
            if (!this.existBuildings(passageDTO.floor1.buildingCode, passageDTO.floor2.buildingCode)) {
                return Result.fail<IPassageDTO>('Building not found')
            }

            //verify if floors exist
            const floor1 = await this.floorRepo.findByCodeNumber(
                BuildingCode.create(passageDTO.floor1.buildingCode).getValue(),
                FloorNumber.create(passageDTO.floor1.floorNumber).getValue(),
            )
            const floor2 = await this.floorRepo.findByCodeNumber(
                BuildingCode.create(passageDTO.floor2.buildingCode).getValue(),
                FloorNumber.create(passageDTO.floor2.floorNumber).getValue(),
            )
            if (floor1 === null || floor2 === null) {
                return Result.fail<IPassageDTO>('Floor not found')
            }

            const passageOrError = Passage.create({ floor1, floor2 })

            if (passageOrError.error) {
                return Result.fail<IPassageDTO>(passageOrError.error.toString())
            }

            if (await this.passageRepo.exists(passageOrError.getValue())) {
                return Result.fail<IPassageDTO>('Passage already exists')
            } else {
                await this.passageRepo.save(passageOrError.getValue())
                const passageDTOResult = PassageMap.toDTO(passageOrError.getValue()) as IPassageDTO
                return Result.ok<IPassageDTO>(passageDTOResult)
            }
        } catch (e) {
            throw e
        }
    }

    private async existBuildings(building1Str: string, building2Str: string): Promise<boolean> {
        const building1 = await this.buildingRepo.findByCode(BuildingCode.create(building1Str).getValue())
        const building2 = await this.buildingRepo.findByCode(BuildingCode.create(building2Str).getValue())

        if (building1 === null || building2 === null) {
            return false
        }
        return true
    }

    public async getAllPassages(): Promise<Result<IPassageDTO[]>> {
        try {
            const passages = await this.passageRepo.findAll()

            if (passages.length === 0) {
                return Result.fail('Buildings not found')
            } else {
                const dtoList = await Promise.all(passages.map((passage) => PassageMap.toDTO(passage)))
                return Result.ok(dtoList)
            }
        } catch (e) {
            throw e
        }
    }

    public async getPassagesBetweenBuildings(
        building1Code: string,
        building2Code: string,
    ): Promise<Result<IPassageDTO[]>> {
        try {
            const b1Code = BuildingCode.create(building1Code)
            const b2Code = BuildingCode.create(building2Code)

            if (b1Code.isFailure || b2Code.isFailure) {
                return Result.fail(b1Code.errorValue())
            }

            const allPassages = await this.passageRepo.findAll()
            const dtoList = await Promise.all(allPassages.map((passage) => PassageMap.toDTO(passage)))

            const passages = await this.passageRepo.passagesBetweenBuildings(
                dtoList,
                b1Code.getValue(),
                b2Code.getValue(),
            )

            if (passages.length === 0) {
                return Result.fail('Buildings not found')
            } else {
                const dtoList = await Promise.all(passages.map((passage) => PassageMap.toDTO(passage)))
                return Result.ok(dtoList)
            }
        } catch (e) {
            throw e
        }
    }
}
