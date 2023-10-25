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
import IUpdatePassageDTO from '../dto/IUpdatePassageDTO'
import { Floor } from '../domain/floor/floor'

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

    private async getPassage(passageDTO: IPassageDTO): Promise<Result<Passage>> {
        const b1 = await this.buildingRepo.findByCode(BuildingCode.create(passageDTO.floor1.buildingCode).getValue())

        if (!b1) {
            return Result.fail(`Building not found with code ${passageDTO.floor1.buildingCode}`)
        }

        const b2 = await this.buildingRepo.findByCode(BuildingCode.create(passageDTO.floor2.buildingCode).getValue())

        if (!b2) {
            return Result.fail(`Building not found with code ${passageDTO.floor2.buildingCode}`)
        }

        const f1 = await this.floorRepo.find(b1, FloorNumber.create(passageDTO.floor1.floorNumber).getValue())

        if (!f1) {
            return Result.fail(
                `Floor ${passageDTO.floor1.floorNumber} not found in building ${passageDTO.floor1.buildingCode}`,
            )
        }

        const f2 = await this.floorRepo.find(b2, FloorNumber.create(passageDTO.floor2.floorNumber).getValue())
        if (!f2) {
            return Result.fail(
                `Floor ${passageDTO.floor2.floorNumber} not found in building ${passageDTO.floor2.buildingCode}`,
            )
        }

        const passage = await this.passageRepo.find(f1, f2)
        if (!passage) {
            // TODO: Floor.toString()?
            return Result.fail(`
                No passage found between ${passageDTO.floor1.buildingCode}-${passageDTO.floor1.floorNumber} and ${passageDTO.floor2.buildingCode}-${passageDTO.floor2.floorNumber}`)
        }

        return Result.ok(passage)
    }

    public async getAllPassages(): Promise<Result<IPassageDTO[]>> {
        try {
            const passages = await this.passageRepo.findAll()

            if (passages.length === 0) {
                return Result.fail('Buildings not found')
            } else {
                const dtoList = await Promise.all(passages.map(passage => PassageMap.toDTO(passage)))
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
            const dtoList = await Promise.all(allPassages.map(passage => PassageMap.toDTO(passage)))

            const passages = await this.passageRepo.passagesBetweenBuildings(
                dtoList,
                b1Code.getValue(),
                b2Code.getValue(),
            )

            if (passages.length === 0) {
                return Result.fail('Buildings not found')
            } else {
                const dtoList = await Promise.all(passages.map(passage => PassageMap.toDTO(passage)))
                return Result.ok(dtoList)
            }
        } catch (e) {
            throw e
        }
    }

    async editPassage(passageDTO: IUpdatePassageDTO): Promise<Result<IPassageDTO>> {
        const passageRes = await this.getPassage(passageDTO.old)
        if (passageRes.isFailure) {
            return Result.fail(passageRes.errorValue())
        }

        const passage = passageRes.getValue()

        let f1: Floor | undefined, f2: Floor | undefined
        if (!!passageDTO.new.floor1) {
            const f1Info = passageDTO.new.floor1

            const b1 = await this.buildingRepo.findByCode(BuildingCode.create(f1Info.buildingCode).getValue())

            if (!b1) {
                return Result.fail(`Building not found with code ${f1Info.buildingCode}`)
            }
            const floor = await this.floorRepo.find(b1, FloorNumber.create(f1Info.floorNumber).getValue())

            if (!floor) {
                return Result.fail(`Floor ${f1Info.floorNumber} not found in building ${f1Info.buildingCode}`)
            }

            f1 = floor
        }

        if (!!passageDTO.new.floor2) {
            const f2Info = passageDTO.new.floor2

            const b2 = await this.buildingRepo.findByCode(BuildingCode.create(f2Info.buildingCode).getValue())

            if (!b2) {
                return Result.fail(`Building not found with code ${f2Info.buildingCode}`)
            }
            const floor = await this.floorRepo.find(b2, FloorNumber.create(f2Info.floorNumber).getValue())

            if (!floor) {
                return Result.fail(`Floor ${f2Info.floorNumber} not found in building ${f2Info.buildingCode}`)
            }

            f2 = floor
        }

        const result = passage.update({
            floor1: f1,
            floor2: f2,
        })

        if (result.isFailure) {
            return Result.fail(result.errorValue())
        }

        const updated = await this.passageRepo.save(result.getValue())
        return Result.ok(PassageMap.toDTO(updated))
    }
}
