import config from '../../config'
import { Service, Inject } from 'typedi'
import { Result } from '../core/logic/Result'

import IBuildingService from './IServices/IBuildingService'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'

import { BuildingMap } from '../mappers/BuildingMap'
import { IBuildingDTO } from '../dto/IBuildingDTO'

import Building  from '../domain/building/building'
import { BuildingCode } from '../domain/building/buildingCode'
import { BuildingName } from '../domain/building/buildingName'
import { Description as BuildingDescription } from '../domain/description'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'

@Service()
export default class BuildingService implements IBuildingService {
    constructor(@Inject(config.repos.building.name) private buildingRepo: IBuildingRepo) {}

    public async createBuilding(dto: IBuildingDTO): Promise<Result<IBuildingDTO>> {
        try {
            /* TODO: error check */
            const code = BuildingCode.create(dto.code).getValue()
            const name = BuildingName.create(dto.name).getValue()
            const description = BuildingDescription.create(dto.description ?? '' /* FIXME */).getValue()

            const { length, width } = dto.maxFloorDimensions
            const maxFloorDimensions = MaxFloorDimensions.create(length, width).getValue()

            const result = Building.create({ code, name, description, maxFloorDimensions })
            if (result.isFailure) {
                return Result.fail<IBuildingDTO>(result.errorValue())
            }

            const building = result.getValue()
            await this.buildingRepo.save(building)

            return Result.ok(BuildingMap.toDTO(building))
        } catch (e) {
            throw e
        }
    }

    public async getBuilding(code: string): Promise<Result<IBuildingDTO>> {
        try {
            const bCode = BuildingCode.create(code)
            if (bCode.isFailure) {
                return Result.fail(bCode.errorValue())
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())
            if (building === null) {
                return Result.fail('Building not found')
            } else {
                return Result.ok(BuildingMap.toDTO(building))
            }
        } catch (e) {
            throw e
        }
    }
}
