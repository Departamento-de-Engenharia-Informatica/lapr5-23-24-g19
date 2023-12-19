import { Service, Inject } from 'typedi'
import config from '../../config'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'
import { IPassageDTO } from '../dto/IPassageDTO'
import IPassageService, { ErrorResult, ErrorCode } from './IServices/IPassageService'
import IPassageRepo from './IRepos/IPassageRepo'
import { Passage } from '../domain/passage/passage'
import { PassageMap } from '../mappers/PassageMap'
import IUpdatePassageDTO from '../dto/IUpdatePassageDTO'
import { Floor } from '../domain/floor/floor'
import { Either, Result, left, right } from '../core/logic/Result'

@Service()
export default class PassageService implements IPassageService {
    constructor(
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.passage.name) private passageRepo: IPassageRepo,
    ) {}

    public async createPassage(
        passageDTO: IPassageDTO,
    ): Promise<Either<ErrorResult, IPassageDTO>> {
        try {
            //verify if building exist
            if (
                !this.existBuildings(
                    passageDTO.floor1.buildingCode,
                    passageDTO.floor2.buildingCode,
                )
            ) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Buildings not found',
                })
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
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Floor Not found',
                })
            } else if (passageDTO.floor1.buildingCode == passageDTO.floor2.buildingCode) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Passage requires different buildings',
                })
            }

            const passageOrError = Passage.create({ floor1, floor2 })

            if (passageOrError.error) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Passage parameters do not meet requirements',
                })
            }

            if (await this.passageRepo.exists(passageOrError.getValue())) {
                return left({
                    errorCode: ErrorCode.AlreadyExists,
                    message: 'Passage already exists',
                })
            } else {
                await this.passageRepo.save(passageOrError.getValue())

                const passageDTOResult = PassageMap.toDTO(
                    passageOrError.getValue(),
                ) as IPassageDTO

                return right(passageDTOResult)
            }
        } catch (e) {
            throw e
        }
    }

    private async existBuildings(
        building1Str: string,
        building2Str: string,
    ): Promise<boolean> {
        const building1 = await this.buildingRepo.findByCode(
            BuildingCode.create(building1Str).getValue(),
        )
        const building2 = await this.buildingRepo.findByCode(
            BuildingCode.create(building2Str).getValue(),
        )

        if (building1 === null || building2 === null) {
            return false
        }
        return true
    }

    private async getPassage(passageDTO: IPassageDTO): Promise<Result<Passage>> {
        const b1 = await this.buildingRepo.findByCode(
            BuildingCode.create(passageDTO.floor1.buildingCode).getValue(),
        )

        if (!b1) {
            return Result.fail(
                `Building not found with code ${passageDTO.floor1.buildingCode}`,
            )
        }

        const b2 = await this.buildingRepo.findByCode(
            BuildingCode.create(passageDTO.floor2.buildingCode).getValue(),
        )

        if (!b2) {
            return Result.fail(
                `Building not found with code ${passageDTO.floor2.buildingCode}`,
            )
        }

        const f1 = await this.floorRepo.find(
            b1,
            FloorNumber.create(passageDTO.floor1.floorNumber).getValue(),
        )

        if (!f1) {
            return Result.fail(
                `Floor ${passageDTO.floor1.floorNumber} not found in building ${passageDTO.floor1.buildingCode}`,
            )
        }

        const f2 = await this.floorRepo.find(
            b2,
            FloorNumber.create(passageDTO.floor2.floorNumber).getValue(),
        )
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

    public async getAllPassages(): Promise<Either<ErrorResult, IPassageDTO[]>> {
        try {
            const passages = await this.passageRepo.findAll()

            if (passages.length === 0) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Passages Not found',
                })
            } else {
                const dtoList = await Promise.all(
                    passages.map((passage) => PassageMap.toDTO(passage)),
                )
                return right(dtoList)
            }
        } catch (e) {
            throw e
        }
    }

    public async getPassagesBetweenBuildings(
        building1Code: string,
        building2Code: string,
    ): Promise<Either<ErrorResult, IPassageDTO[]>> {
        try {
            const b1Code = BuildingCode.create(building1Code)
            const b2Code = BuildingCode.create(building2Code)

            if (b1Code.isFailure) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Building code parameters do not meet requirements',
                })
            } else if (b2Code.isFailure) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Building code parameters do not meet requirements',
                })
            }

            const b1 = await this.buildingRepo.findByCode(b1Code.getValue())
            const b2 = await this.buildingRepo.findByCode(b2Code.getValue())

            if (!b1) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                })
            } else if (!b2) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                })
            }

            if (b1.code.value === b2.code.value) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Same Building',
                })
            }

            const passages = await this.passageRepo.passagesBetweenBuildings(b1, b2)

            if (passages.length === 0) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Passages not found',
                })
            } else {
                const dtoList = await Promise.all(
                    passages.map((passage) => PassageMap.toDTO(passage)),
                )
                return right(dtoList)
            }
        } catch (e) {
            throw e
        }
    }

    async editPassage(
        passageDTO: IUpdatePassageDTO,
    ): Promise<Either<ErrorResult, IPassageDTO>> {
        const passageRes = await this.getPassage(passageDTO.old)
        if (passageRes.isFailure) {
            return left({
                errorCode: ErrorCode.NotFound,
                message: 'Passages not found',
            })
        }

        const passage = passageRes.getValue()
        let f1: Floor | undefined, f2: Floor | undefined
        if (!!passageDTO.new.floor1) {
            const f1Info = passageDTO.new.floor1

            const b1 = await this.buildingRepo.findByCode(
                BuildingCode.create(f1Info.buildingCode).getValue(),
            )

            if (!b1) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                })
            }
            const floor = await this.floorRepo.find(
                b1,
                FloorNumber.create(f1Info.floorNumber).getValue(),
            )

            if (!floor) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Floor not found ',
                })
            }

            f1 = floor
        }

        if (!!passageDTO.new.floor2) {
            const f2Info = passageDTO.new.floor2
            const b2 = await this.buildingRepo.findByCode(
                BuildingCode.create(f2Info.buildingCode).getValue(),
            )

            if (!b2) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                })
            }

            const floor = await this.floorRepo.find(
                b2,
                FloorNumber.create(f2Info.floorNumber).getValue(),
            )

            if (!floor) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Floor not found',
                })
            }

            f2 = floor
        }
        const result = passage.update({
            floor1: f1,
            floor2: f2,
        })

        if (result.isFailure) {
            return left({
                errorCode: ErrorCode.BussinessRuleViolation,
                message: 'Passage between same building/floors not allowed',
            })
        }
        const updated = await this.passageRepo.save(result.getValue())
        return right(PassageMap.toDTO(updated))
    }
}
