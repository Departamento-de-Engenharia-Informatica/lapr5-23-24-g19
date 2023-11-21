import { Inject, Service } from 'typedi'
import config from '../../config'
import { Either, left, right } from '../core/logic/Result'
import { BuildingCode } from '../domain/building/code'
import { Floor } from '../domain/floor/floor'
import { FloorNumber } from '../domain/floor/floorNumber'
import { IBuildingFloorDTO } from '../dto/IBuildingFloorDTO'
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { FloorMap } from '../mappers/FloorMap'
import { IStorageFs } from './IFs/IStorageFs'
import IFloorRepo from './IRepos/IFloorRepo'
import IFloorMapService, { ErrorCode, ErrorResult } from './IServices/IFloorMapService'

@Service()
export default class FloorMapService implements IFloorMapService {
    constructor(
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.storage.name) private storage: IStorageFs,
    ) {}

    async uploadMap(dto: IFloorMapDTO): Promise<Either<ErrorResult, IFloorMapDTO>> {
        try {
            const floor = await this.floorRepo.findByCodeNumber(
                BuildingCode.create(dto.buildingCode).getOrThrow(),
                FloorNumber.create(dto.floorNumber).getValue(),
            )
            if (floor === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: 'Floor not found',
                })
            }

            if (!this.mapFits(dto, floor)) {
                return left({
                    errorCode: ErrorCode.BusinessRuleViolation,
                    message: 'Map does not meet requirements',
                })
            }

            const filename = this.makePath(floor)

            const path = await this.storage.upload(filename, dto)
            floor.addMap(path)

            const f = await this.floorRepo.save(floor)
            return right(FloorMap.toDTOFloorMap(f))
        } catch (e) {
            throw e
        }
    }

    async getMap(dto: IBuildingFloorDTO): Promise<Either<ErrorResult, IFloorMapDTO>> {
        const floor = await this.floorRepo.findByCodeNumber(
            BuildingCode.create(dto.buildingCode).getOrThrow(),
            FloorNumber.create(dto.floorNumber).getValue(),
        )
        if (floor === null) {
            return left({
                errorCode: ErrorCode.NotFound,
                message: 'Floor not found',
            })
        } else if (!floor.mapPath) {
            return left({
                errorCode: ErrorCode.NotFound,
                message: 'Floor does not contain a map',
            })
        }

        const map = await this.storage.get<IFloorMapDTO>(floor.mapPath)
        return right(map)
    }

    private mapFits(dto: IFloorMapDTO, floor: Floor): boolean {
        const { width, length } = floor.building.maxFloorDimensions
        return dto.map!.dimensions.length <= length && dto.map!.dimensions.width <= width
    }

    private makePath(floor: Floor): string {
        const code = floor.building.code.value
        const number = floor.floorNumber.value
        return `maps/${code}/${number}.json`
    }
}
