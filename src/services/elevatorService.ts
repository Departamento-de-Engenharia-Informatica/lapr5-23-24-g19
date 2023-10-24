import { Inject, Service } from 'typedi'
import config from '../../config'
import { Result } from '../core/logic/Result'
import { BuildingCode } from '../domain/building/buildingCode'
import Elevator from '../domain/elevator/Elevator'
import { ElevatorIdentifier } from '../domain/elevator/identifier'
import { FloorNumber } from '../domain/floor/floorNumber'
import { IElevatorDTO } from '../dto/IElevatorDTO'
import { ElevatorMap } from '../mappers/ElevatorMap'
import IBuildingRepo from './IRepos/IBuildingRepo'
import IElevatorRepo from './IRepos/IElevatorRepo'
import IFloorRepo from './IRepos/IFloorRepo'
import IElevatorService from './IServices/IElevatorService'
import { ElevatorBrand } from '../domain/elevator/brand'
import { ElevatorModel } from '../domain/elevator/model'
import { ElevatorSerialNumber } from '../domain/elevator/serialNumber'
import { ElevatorDescription } from '../domain/elevator/description'
import MongoElevatorDataMap from '../repos/mongo/dataMapper/ElevatorDataMap'
import Building from '../domain/building/building'
import { BuildingMap } from '../mappers/BuildingMap'

@Service()
export default class ElevatorService implements IElevatorService {
    constructor(
        @Inject(config.repos.elevator.name) private repo: IElevatorRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    async createElevator(dto: IElevatorDTO): Promise<Result<IElevatorDTO>> {
        const building = await this.buildingRepo.findByCode(BuildingCode.create(dto.buildingId).getValue())

        if (!building) {
            return Result.fail('Building not found')
        }

        const identifier = ElevatorIdentifier.create(dto.identifier).getValue()

        if (await this.repo.existsInBuilding(building, identifier)) {
            return Result.fail(`Elevator already exists with identifier ${identifier.value}`)
        }

        // grab the floors by their number in the building
        const floors =
            (
                await Promise.all(
                    dto.floors.map(async fNum => {
                        const num = FloorNumber.create(fNum).getValue()
                        return (await this.floorRepo.find(building, num)) ?? undefined
                    }),
                )
            ).filter(f => f !== null && f !== undefined) ?? []

        const result = Elevator.create({
            building,
            identifier,
            floors,
        })

        if (result.isFailure) {
            return Result.fail(result.error)
        }

        const elevator = result.getValue()
        await this.repo.save(elevator)
        return Result.ok(ElevatorMap.toDTO(elevator))
    }

    public async editElevator(identifier: string, dto: IElevatorDTO): Promise<Result<IElevatorDTO>> {
        try {
            const elevatorIdentifier = ElevatorIdentifier.create(parseInt(identifier))

            if (elevatorIdentifier.isFailure) {
                return Result.fail(elevatorIdentifier.errorValue())
            }

            const building = await this.buildingRepo.findByCode(BuildingCode.create(dto.buildingId).getValue())

            const existElevator = await this.repo.existsInBuilding(building, elevatorIdentifier.getValue())

            if (existElevator === null) {
                return Result.fail('Elevator not found')
            }

            const elevator = await this.repo.findByIdentifier(building, elevatorIdentifier.getValue())

            /* if (dto.brand) {
                elevator.brand = ElevatorBrand.create(dto.brand).getValue()
            }
            if (dto.model) {
                elevator.model = ElevatorModel.create(dto.model).getValue()
            }
            if (dto.serialNumber) {
                elevator.serialNumber = ElevatorSerialNumber.create(dto.serialNumber).getValue()
            }
            if (dto.description) {
                elevator.description = ElevatorDescription.create(dto.description).getValue()
            }
            if (dto.floors) {
                elevator.floors = dto.floors
            }*/

            const elevatorRes = await this.repo.save(elevator)

            return Result.ok(ElevatorMap.toDTO(elevatorRes))
        } catch (e) {
            throw e
        }
    }

    public async getElevators(code: string): Promise<Result<IElevatorDTO[]>> {
        try {
            const bCode = BuildingCode.create(code)

            if (bCode.isFailure) {
                return Result.fail(bCode.errorValue())
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())

            if (building === null) {
                return Result.fail('Building not found')
            }

            const elevators = await this.repo.inBuilding(building)

            if (elevators.length === 0) {
                return Result.fail('Elevators not found')
            } else {
                const dtoList = await Promise.all(elevators.map(elevator => ElevatorMap.toDTO(elevator)))
                return Result.ok(dtoList)
            }
        } catch (e) {
            throw e
        }
    }
}
