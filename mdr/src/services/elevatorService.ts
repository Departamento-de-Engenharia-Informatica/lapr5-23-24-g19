import { Inject, Service } from 'typedi'
import config from '../../config'
import { Either, Result, left, right } from '../core/logic/Result'

import IElevatorService, { ErrorResult, ErrorCode } from './IServices/IElevatorService'
import IElevatorRepo from './IRepos/IElevatorRepo'
import { ElevatorMap } from '../mappers/ElevatorMap'

import IBuildingRepo from './IRepos/IBuildingRepo'
import IFloorRepo from './IRepos/IFloorRepo'

import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'

import Elevator from '../domain/elevator/Elevator'
import { ElevatorBrand as Brand } from '../domain/elevator/brand'
import { ElevatorDescription as Description } from '../domain/elevator/description'
import { ElevatorModel as Model } from '../domain/elevator/model'
import { ElevatorSerialNumber as SerialNumber } from '../domain/elevator/serialNumber'
import { IElevatorDTO } from '../dto/IElevatorDTO'
import { ICreatedElevatorDTO } from '../dto/ICreatedElevatorDTO'
import { ElevatorIdentifier as Identifier } from '../domain/elevator/identifier'

@Service()
export default class ElevatorService implements IElevatorService {
    constructor(
        @Inject(config.repos.elevator.name) private repo: IElevatorRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    async createElevator(
        dto: IElevatorDTO,
    ): Promise<Either<ErrorResult, ICreatedElevatorDTO>> {
        try {
            const building = await this.buildingRepo.findByCode(
                BuildingCode.create(dto.buildingId).getValue(),
            )

            if (!building) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building Not found',
                })
            }
            const identifier = await this.repo.nextIdentifier()

            // map the floorId's into the respective Floor object
            const floors =
                (
                    await Promise.all(
                        dto.floors.map(async (fNum) => {
                            const num = FloorNumber.create(fNum).getOrThrow()
                            try {
                                return await this.floorRepo.find(building, num)
                            } catch (_) {
                                return undefined
                            }
                        }),
                    )
                ).filter((f) => f !== null && f !== undefined) ?? []

            if (floors.length < dto.floors.length) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Floors were not found ',
                })
            }

            const brand = dto.brand && Brand.create(dto.brand).getOrThrow()
            const model = dto.model && Model.create(dto.model).getOrThrow()
            const serialNumber =
                dto.serialNumber && SerialNumber.create(dto.serialNumber).getOrThrow()
            const description =
                dto.description && Description.create(dto.description).getOrThrow()

            const result = Elevator.create({
                building,
                identifier,
                floors,

                brand,
                model,
                serialNumber,
                description,
            })

            if (result.isFailure) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Brand cannot exist without specifying model ',
                })
            }

            const elevator = result.getOrThrow()
            await this.repo.save(elevator)

            return right(ElevatorMap.toDTO(elevator))
        } catch (e) {
            return left({
                errorCode: ErrorCode.BussinessRuleViolation,
                message: e.message ?? 'Business rule violation',
            } as ErrorResult)
        }
    }

    public async editElevator(
        identifier: number,
        dto: IElevatorDTO,
    ): Promise<Either<ErrorResult, ICreatedElevatorDTO>> {
        try {
            const building = await this.buildingRepo.findByCode(
                BuildingCode.create(dto.buildingId).getValue(),
            )

            if (!building) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building Not found',
                })
            }

            const elevatorIdentifier = Identifier.create(identifier).getValue()

            const existElevator = await this.repo.existsInBuilding(
                building,
                elevatorIdentifier,
            )

            if (existElevator === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Elevator Not found',
                })
            }

            const elevator = await this.repo.findByIdentifier(
                building,
                elevatorIdentifier,
            )

            elevator.brand = dto.brand ? Brand.create(dto.brand).getValue() : null
            elevator.model = dto.model ? Model.create(dto.model).getValue() : null
            elevator.serialNumber = dto.serialNumber
                ? SerialNumber.create(dto.serialNumber).getValue()
                : null
            elevator.description = dto.description
                ? Description.create(dto.description).getValue()
                : null

            if (dto.floors) {
                const floors =
                    (
                        await Promise.all(
                            dto.floors.map(async (fNum) => {
                                const num = FloorNumber.create(fNum).getValue()
                                try {
                                    return await this.floorRepo.find(building, num)
                                } catch (_) {
                                    return undefined
                                }
                            }),
                        )
                    ).filter((f) => f !== null && f !== undefined) ?? []

                if (floors.length < dto.floors.length) {
                    return left({
                        errorCode: ErrorCode.NotFound,
                        message: 'Floors were not found ',
                    })
                }

                elevator.floors = floors
            }

            const elevatorRes = await this.repo.save(elevator)
            return right(ElevatorMap.toDTO(elevatorRes))
        } catch (e) {
            return left({
                errorCode: ErrorCode.BussinessRuleViolation,
                message: e.message ?? 'Business rule violation',
            } as ErrorResult)
            throw e
        }
    }

    public async getElevators(
        code: string,
    ): Promise<Either<ErrorResult, ICreatedElevatorDTO[]>> {
        try {
            const bCode = BuildingCode.create(code)

            if (bCode.isFailure) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: 'Building code parameters do not meet requirements',
                })
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())

            if (building === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Building not found',
                })
            }

            const elevators = await this.repo.inBuilding(building)

            if (elevators.length === 0) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Elevators not found',
                })
            } else {
                const dtoList = await Promise.all(
                    elevators.map((elevator) => ElevatorMap.toDTO(elevator)),
                )
                return right(dtoList)
            }
        } catch (e) {
            throw e
        }
    }
}
