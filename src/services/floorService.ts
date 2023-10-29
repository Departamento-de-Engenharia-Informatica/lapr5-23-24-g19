import { IFloorDTO } from '../dto/IFloorDTO'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'
import IFloorService from './IServices/IFloorService'
import { Either, left, Result, right } from '../core/logic/Result'
import { Floor } from '../domain/floor/floor'
import { FloorMap } from '../mappers/FloorMap'
import { BuildingCode } from '../domain/building/buildingCode'
import { FloorNumber } from '../domain/floor/floorNumber'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'
import { Description } from '../domain/description'
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { IBuildingCodeDTO } from '../dto/IBuildingCodeDTO'
import IPassageRepo from './IRepos/IPassageRepo'
import { IUpdateFloorDTO } from '../dto/IUpdateFloorDTO'
import { IFloorPassageDTO } from '../dto/IFloorPassageDTO'
import { FloorPassageMap } from '../mappers/FloorPassageMap'
import { ErrorCode, ErrorResult } from './IServices/IFloorService'
import config from '../../config'
import { Inject, Service } from 'typedi'

@Service()
export default class FloorService implements IFloorService {
    constructor(
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.passage.name) private passageRepo: IPassageRepo,
    ) {}

    public async createFloor(floorDTO: IFloorDTO, buildingCode: string): Promise<Either<ErrorResult, IFloorDTO>> {
        try {
            const code = BuildingCode.create(buildingCode).getValue()
            const building = await this.buildingRepo.findByCode(code)

            if (!building) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building not found"
                })
            }

            const floorNumber = FloorNumber.create(floorDTO.floorNumber).getValue()
            const floor = await this.floorRepo.findByCodeNumber(code, floorNumber)

            if (floor) {
                return left({
                    errorCode: ErrorCode.BusinessRuleViolation,
                    message: "Floor already exists"
                })
            }

            floorDTO.buildingCode = buildingCode

            let description = undefined

            if (floorDTO.description) {
                description = Description.create(floorDTO.description)

                if (description.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: "Description not valid"
                    })
                }
                description = description.getValue()
            }

            const floorOrError = Floor.create({
                building: building,
                floorNumber: FloorNumber.create(floorDTO.floorNumber).getValue(),
                description: description
            })

            if (floorOrError.error) {
                return left({
                    errorCode: ErrorCode.BusinessRuleViolation,
                    message: "There was a problem while create the floor"
                })
            }

            const result = await this.floorRepo.save(floorOrError.getValue())
            return right(FloorMap.toDTO(result))
        } catch (e) {
            throw e
        }
    }

    public async patchFloor(dto: IUpdateFloorDTO): Promise<Either<ErrorResult, IFloorDTO>> {
        try {
            const buildingCode = BuildingCode.create(dto.buildingCode).getValue()
            const building = await this.buildingRepo.findByCode(buildingCode)

            if (!building) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building not found"
                })
            }

            const oldFloorNumber = FloorNumber.create(dto.oldFloorNumber).getValue()

            const floor = await this.floorRepo.findByCodeNumber(buildingCode, oldFloorNumber)

            if (floor === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Floor not found"
                })
            }

            if (dto.description) {
                const description = Description.create(dto.description)

                if (description.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: "Floor description do not meet requirements"
                    })
                }

                floor.description = description.getValue()
            }

            if (dto.floorNumber) {
                const newFloorNumber = FloorNumber.create(dto.floorNumber)

                if (newFloorNumber.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: "Floor number do not meet requirements"
                    })
                }

                floor.floorNumber = newFloorNumber.getValue()
            }

            const result = await this.floorRepo.save(floor)
            return right(FloorMap.toDTO(result))
        } catch (e) {
            throw e
        }
    }

    public async putFloor(dto: IUpdateFloorDTO): Promise<Either<ErrorResult, IFloorDTO>> {
        try {
            const buildingCode = BuildingCode.create(dto.buildingCode).getValue()
            const building = await this.buildingRepo.findByCode(buildingCode)

            if (!building) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building not found"
                })
            }

            const oldFloorNumber = FloorNumber.create(dto.oldFloorNumber).getValue()

            const floor = await this.floorRepo.findByCodeNumber(buildingCode, oldFloorNumber)

            if (floor === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Floor not found"
                })
            }

            const description = dto.description && Description.create(dto.description)

            if (description.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: "Floor description do not meet requirements"
                    })
            }

            floor.description = description.getValue()

            if (dto.floorNumber) {
                const newFloorNumber = FloorNumber.create(dto.floorNumber)

                if (newFloorNumber.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: "Floor number do not meet requirements"
                    })
                }

                floor.floorNumber = newFloorNumber.getValue()
            }

            const result = await this.floorRepo.save(floor)
            return right(FloorMap.toDTO(result))
        } catch (e) {
            throw e
        }
    }

    public async getFloors(buildingCode: string): Promise<Either<ErrorResult, IFloorDTO[]>> {
        try {
            const code = BuildingCode.create(buildingCode).getValue()
            const building = await this.buildingRepo.findByCode(code)

            if (!building) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building not found"
                })
            }

            const floors = await this.floorRepo.findByBuildingCode(code)
            console.log(floors)

            if (floors.length == 0) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Floors not found"
                })
            }

            const dtoList = await Promise.all(floors.map((floor) => FloorMap.toDTO(floor)))
            return right(dtoList)
        } catch (e) {
            throw e
        }
    }

    public async uploadMap(dto: IFloorMapDTO): Promise<Result<IFloorMapDTO>> {
        try {
            const floor = await this.floorRepo.findByCodeNumber(
                BuildingCode.create(dto.buildingCode).getValue(),
                FloorNumber.create(dto.floorNumber).getValue(),
            )

            if (floor === null) {
                return Result.fail<IFloorMapDTO>('Floor not found')
            }

            if (
                !floor.building.fit(MaxFloorDimensions.create(dto.dimensions.length, dto.dimensions.width).getValue())
            ) {
                return Result.fail<IFloorMapDTO>('Dimensions not valid')
            }

            if (!floor.addMap(dto)) {
                return Result.fail<IFloorMapDTO>('Map could not be created')
            }
            const saved = await this.floorRepo.save(floor)
            return Result.ok<IFloorMapDTO>(FloorMap.toDTOFloorMap(saved))
        } catch (e) {
            throw e
        }
    }

    async floorsWithPassage(buildingDTO: IBuildingCodeDTO): Promise<Result<IFloorPassageDTO[]>> {
        const building = await this.buildingRepo.findByCode(BuildingCode.create(buildingDTO.code).getValue())

        if (!building) {
            return Result.fail(`Building with code ${buildingDTO.code} does not exist`)
        }

        const floors = await this.passageRepo.floorsWithPassage(building)
        return Result.ok(floors.map(f => FloorPassageMap.toDTO(f)))
    }
}
