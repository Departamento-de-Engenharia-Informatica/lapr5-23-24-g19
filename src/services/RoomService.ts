import config from '../../config'
import { Service, Inject } from 'typedi'
import { Result } from '../core/logic/Result'

import IBuildingService from './IServices/IBuildingService'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'

import { BuildingMap } from '../mappers/BuildingMap'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { IBuildingEditDTO } from '../dto/IBuildingEditDTO'

import Building from '../domain/building/building'
import { BuildingCode } from '../domain/building/buildingCode'
import { BuildingName } from '../domain/building/buildingName'
import { BuildingDescription } from '../domain/building/description'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'
import { IBuildingMinMaxFloorsDTO } from '../dto/IBuildingMinMaxFloorsDTO'
import { IBuildingFloorNumberDTO } from '../dto/IBuildingFloorNumberDTO'
import IFloorRepo from './IRepos/IFloorRepo'
import { Resolver } from 'dns'
import { BuildingFloorNumberMap } from '../mappers/BuildingFloorNumberMap'
import { IRoomDTO } from '../dto/IRoomDTO'
import IRoomService from './IServices/IRoomService'
import IRoomRepo from './IRepos/IRoomRepo'
import { RoomName } from '../domain/room/roomName'
import { RoomDescription } from '../domain/room/description'
import { RoomCategory } from '../domain/room/roomCategory'

@Service()
export default class RoomService implements IRoomService {
    constructor(
        @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    public async createRoom(dto: IRoomDTO): Promise<Result<IRoomDTO>> {
        const buildingCode = BuildingCode.create(dto.buildingCode)

        const name = RoomName.create(dto.name).getValue()
        const description = RoomDescription.create(dto.description).getValue()
        const category = RoomCategory.create(dto.category).getValue()

        return null
    }
}
