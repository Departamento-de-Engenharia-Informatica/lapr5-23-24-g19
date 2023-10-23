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
                    dto.floors.map(async (fNum) => {
                        const num = FloorNumber.create(fNum).getValue()
                        return (await this.floorRepo.find(building, num)) ?? undefined
                    }),
                )
            ).filter((f) => f !== null && f !== undefined) ?? []

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
}
