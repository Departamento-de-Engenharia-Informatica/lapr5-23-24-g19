import { Inject, Service } from 'typedi'
import config from '../../config'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'
import { RoomName } from '../domain/room/roomName'
import { IGetPathsDTO } from '../dto/IGetPathsDTO'
import { IPathBetweenBuildingsDTO } from '../dto/IPathBetweenBuildingsDTO'
import { IPathCriterionDTO } from '../dto/IPathCriterionDTO'
import { IPathDTO } from '../dto/IPathDTO'
import IPlanningAdapter from './IRepos/IPathRepo'
import IRoomRepo from './IRepos/IRoomRepo'
import IPathService from './IServices/IPathService'

@Service()
export default class PathService implements IPathService {
    constructor(
        @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
        @Inject(config.repos.path.name) private pathRepo: IPlanningAdapter,
    ) {}

    async pathsBetweenBuildings(dto: IGetPathsDTO): Promise<IPathDTO[]> {
        const roomStart = await this.getRoom(
            dto.roomStart.building,
            dto.roomStart.floor,
            dto.roomStart.name,
        )

        const roomGoal = await this.getRoom(
            dto.roomGoal.building,
            dto.roomGoal.floor,
            dto.roomGoal.name,
        )

        const pathDto: IPathBetweenBuildingsDTO = {
            criteria: dto.criteria,
            start: {
                building: dto.roomStart.building,
                floor: dto.roomStart.floor,
                coordinates: {
                    x: roomStart.positions.x,
                    y: roomStart.positions.y,
                },
            },
            goal: {
                building: dto.roomGoal.building,
                floor: dto.roomGoal.floor,
                coordinates: {
                    x: roomGoal.positions.x,
                    y: roomGoal.positions.y,
                },
            },
        }

        return this.pathRepo.find(pathDto)
    }

    async getPathCriteria(): Promise<IPathCriterionDTO[]> {
        return this.pathRepo.criteria()
    }

    private async getRoom(building: string, floor: number, name: string) {
        const bCode = BuildingCode.create(building).getOrThrow()
        const floorNum = FloorNumber.create(floor).getOrThrow()
        const roomName = RoomName.create(name).getOrThrow()

        return this.roomRepo.find(bCode, floorNum, roomName)
    }
}
