import config from '../../config'
import { Service, Inject } from 'typedi'
import { BuildingCode } from '../domain/building/buildingCode'
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

@Service()
export default class RoomService implements IRoomService {
    constructor(
        @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    public async createRoom(dto: IRoomDTO): Promise<Either<ErrorResult, IRoomDTO>> {
        const buildingCode = BuildingCode.create(dto.buildingCode).getValue()
        const floorNumber = FloorNumber.create(dto.floorNumber).getValue()

        const floor = await this.floorRepo.findByCodeNumber(buildingCode, floorNumber)
        if (!floor) {
            return left({
                errorCode: ErrorCode.NotFound,
                message: 'Floor Not found',
            })
        }

        try {
            const dtoName = RoomName.create(dto.name).getValue()
            const dtoDescription = RoomDescription.create(dto.description).getValue()
            const dtoCategory = RoomCategory.create(dto.category).getValue()
            const dtoDimensions = RoomDimensions.create(dto.dimensions.length, dto.dimensions.width).getValue()
            const dtoPositions = Coordinates.create(dto.positions.x, dto.positions.y).getValue()

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
                message: 'Business rule violation',
            } as ErrorResult)
            throw e
        }
    }
}
