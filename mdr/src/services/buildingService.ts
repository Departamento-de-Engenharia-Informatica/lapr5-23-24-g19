import config from '../../config'
import { Service, Inject } from 'typedi'
import { Either, left, right } from '../core/logic/Result'

import IBuildingService, { ErrorCode, ErrorResult } from './IServices/IBuildingService'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'

import { BuildingMap } from '../mappers/BuildingMap'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { IBuildingEditDTO } from '../dto/IBuildingEditDTO'

import Building from '../domain/building/building'
import { BuildingCode as Code } from '../domain/building/code'
import { BuildingName as Name } from '../domain/building/name'
import { BuildingDescription } from '../domain/building/description'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'
import { IBuildingMinMaxFloorsDTO } from '../dto/IBuildingMinMaxFloorsDTO'
import IFloorRepo from './IRepos/IFloorRepo'
import { BuildingFloorNumberMap } from '../mappers/BuildingFloorNumberMap'
import { IBuildingFloorNumberDTO } from '../dto/IBuildingFloorNumberDTO'

@Service()
export default class BuildingService implements IBuildingService {
    constructor(
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    public async createBuilding(
        dto: IBuildingDTO,
    ): Promise<Either<ErrorResult, IBuildingDTO>> {
        try {
            const code = Code.create(dto.code).getOrThrow()
            const name = dto.name && Name.create(dto.name).getOrThrow()
            const description =
                dto.description &&
                BuildingDescription.create(dto.description).getOrThrow()

            const { length, width } = dto.maxFloorDimensions
            const maxFloorDimensions = MaxFloorDimensions.create(
                length,
                width,
            ).getOrThrow()

            const result = Building.create({
                code,
                name,
                description,
                maxFloorDimensions,
            })
            if (result.isFailure) {
                return left({
                    errorCode: ErrorCode.BusinessRuleViolation,
                    message: 'Building parameters do not meet requirements',
                })
            }

            if (!!(await this.buildingRepo.findByCode(code))) {
                return left({
                    errorCode: ErrorCode.AlreadyExists,
                    message: 'Building already exists',
                })
            }

            const building = result.getValue()
            const saved = await this.buildingRepo.save(building)

            return right(BuildingMap.toDTO(saved))
        } catch (e) {
            return left({
                errorCode: ErrorCode.BusinessRuleViolation,
                message: e.message,
            })
        }
    }

    public async getBuilding(code: string): Promise<Either<ErrorResult, IBuildingDTO>> {
        try {
            const bCode = Code.create(code)
            if (bCode.isFailure) {
                return left({
                    errorCode: ErrorCode.BusinessRuleViolation,
                    message: 'Building code not valid',
                })
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())
            if (building === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                })
            } else {
                return right(BuildingMap.toDTO(building))
            }
        } catch (e) {
            throw e
        }
    }

    public async getBuildings(): Promise<Either<ErrorResult, IBuildingDTO[]>> {
        try {
            const buildings = await this.buildingRepo.findAll()

            if (buildings.length === 0) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Buildings not found',
                })
            } else {
                const dtoList = buildings.map((building) => BuildingMap.toDTO(building))
                return right(dtoList)
            }
        } catch (e) {
            throw e
        }
    }

    public async getBuildingsByFloors(
        dto: IBuildingMinMaxFloorsDTO,
    ): Promise<Either<ErrorResult, IBuildingFloorNumberDTO[]>> {
        try {
            const buildingsAndFloorCount =
                await this.floorRepo.findBuildingsByMinMaxFloors(
                    dto.minMaxFloors.min,
                    dto.minMaxFloors.max,
                )

            if (buildingsAndFloorCount.length == 0) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Buildings not found',
                })
            }

            const dtoList = await Promise.all(
                buildingsAndFloorCount.map(async (value) => {
                    const building = await this.buildingRepo.findByCode(
                        value.buildingCode,
                    )
                    return BuildingFloorNumberMap.toDTO(building, value.floorCount)
                }),
            )

            return right(dtoList)
        } catch (e) {
            throw e
        }
    }
    public async editBuilding(
        dto: IBuildingEditDTO,
    ): Promise<Either<ErrorResult, IBuildingDTO>> {
        try {
            const bCode = Code.create(dto.code)
            if (bCode.isFailure) {
                return left({
                    errorCode: ErrorCode.BusinessRuleViolation,
                    message: 'Building Code does not meet requirements',
                } as ErrorResult)
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())

            // TODO: refactor into Building.update()
            if (building === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                } as ErrorResult)
            }

            if (dto.name) {
                const nameEdit = Name.create(dto.name)
                if (nameEdit.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: 'Building Name does not meet requirements',
                    } as ErrorResult)
                }
                building.name = nameEdit.getValue()
            }

            if (dto.description) {
                const descrEdit = BuildingDescription.create(dto.description)
                if (descrEdit.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: 'Building Descr does not meet requirements',
                    } as ErrorResult)
                }
                building.description = descrEdit.getValue()
            }

            if (dto.maxFloorDimensions) {
                const length = dto.maxFloorDimensions.length
                const width = dto.maxFloorDimensions.width

                const maxFloor = MaxFloorDimensions.create(length, width)
                if (maxFloor.isFailure) {
                    return left({
                        errorCode: ErrorCode.BusinessRuleViolation,
                        message: 'Building Dimensios does not meet requirements',
                    } as ErrorResult)
                }
                building.maxFloorDimensions = maxFloor.getValue()
            }

            const buildingRes = await this.buildingRepo.save(building)

            return right(BuildingMap.toDTO(buildingRes))
        } catch (e) {
            return left({
                errorCode: ErrorCode.BusinessRuleViolation,
                message: 'Business rule violation',
            } as ErrorResult)
        }
    }
}
