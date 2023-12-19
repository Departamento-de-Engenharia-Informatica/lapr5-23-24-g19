import config from '../../config'
import { Service, Inject } from 'typedi'
import { BuildingCode } from '../domain/building/code'
import IFloorRepo from './IRepos/IFloorRepo'
import { IRoomDTO } from '../dto/IRoomDTO'
import IRoomService, { ErrorResult, ErrorCode } from './IServices/IRoomService'
import IRoomRepo from './IRepos/IRoomRepo'
import { RoomName } from '../domain/room/roomName'
import { RoomDescription } from '../domain/room/description'
import { RoomCategory } from '../domain/room/roomCategory'
import { FloorNumber } from '../domain/floor/floorNumber'
import Room from '../domain/room/room'
import { RoomDimensions } from '../domain/room/roomDimensions'
import { Coordinates } from '../domain/floor/Coordinates'
import { RoomMap } from '../mappers/RoomMap'
import { Either, Result, left, right } from '../core/logic/Result'
import { ICreatedElevatorDTO } from '../dto/ICreatedElevatorDTO'
import { ElevatorMap } from '../mappers/ElevatorMap'
import IBuildingRepo from './IRepos/IBuildingRepo'

@Service()
export default class RoomService implements IRoomService {
    constructor(
        @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    public async createRoom(dto: IRoomDTO): Promise<Either<ErrorResult, IRoomDTO>> {
        try {
            const buildingCode = BuildingCode.create(dto.buildingCode).getOrThrow()
            const floorNumber = FloorNumber.create(dto.floorNumber).getOrThrow()

            const floor = await this.floorRepo.findByCodeNumber(buildingCode, floorNumber)
            if (!floor) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Floor Not found',
                })
            }

            const dtoName = RoomName.create(dto.name).getOrThrow()
            const dtoDescription = RoomDescription.create(dto.description).getOrThrow()
            const dtoCategory = RoomCategory.create(dto.category).getOrThrow()
            const dtoDimensions = RoomDimensions.create(
                dto.dimensions.length,
                dto.dimensions.width,
            ).getOrThrow()
            const dtoPositions = Coordinates.create(
                dto.positions.x,
                dto.positions.y,
            ).getOrThrow()

            const room = Room.create({
                name: dtoName,
                description: dtoDescription,
                floor: floor,
                category: dtoCategory,
                dimensions: dtoDimensions,
                positions: dtoPositions,
            }).getValue()

            if (await this.roomRepo.exists(room)) {
                return left({
                    errorCode: ErrorCode.AlreadyExists,
                    message: 'Room already exists ',
                })
            }

            const saved = await this.roomRepo.save(room)

            return right(RoomMap.toDTO(saved))
        } catch (e) {
            return left({
                errorCode: ErrorCode.BussinessRuleViolation,
                message: e.message ?? 'Business rule violation',
            } as ErrorResult)
            throw e
        }
    }

    public async getRooms(dto: IRoomDTO): Promise<Either<ErrorResult, IRoomDTO[]>> {
        try {
            const bCode = BuildingCode.create(dto.buildingCode)

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

            const fNumber = FloorNumber.create(dto.floorNumber)

            const floor = await this.floorRepo.find(building, fNumber.getValue())

            if (floor === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'floor not found',
                })
            }

            const rooms = await this.roomRepo.findAllInFloor(building, floor)

            const dtoList = await Promise.all(rooms.map((room) => RoomMap.toDTO(room)))
            return right(dtoList)
        } catch (e) {
            throw e
        }
    }
}
