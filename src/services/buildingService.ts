import config from '../../config'
import { Service, Inject } from 'typedi'
import { Either, Result,left,right } from '../core/logic/Result'

import IBuildingService, { ErrorCode, ErrorResult } from './IServices/IBuildingService'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'

import { BuildingMap } from '../mappers/BuildingMap'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { IBuildingEditDTO } from '../dto/IBuildingEditDTO'

import Building  from '../domain/building/building'
import { BuildingCode } from '../domain/building/buildingCode'
import { BuildingName } from '../domain/building/buildingName'
import { BuildingDescription } from '../domain/building/description'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'
import { IBuildingMinMaxFloorsDTO } from '../dto/IBuildingMinMaxFloorsDTO'
import { IBuildingFloorNumberDTO } from '../dto/IBuildingFloorNumberDTO'
import IFloorRepo from './IRepos/IFloorRepo'
import { Resolver } from 'dns'
import { BuildingFloorNumberMap } from '../mappers/BuildingFloorNumberMap'

@Service()
export default class BuildingService implements IBuildingService {
    constructor(@Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
                @Inject(config.repos.floor.name) private floorRepo: IFloorRepo) {}

    public async createBuilding(dto: IBuildingDTO): Promise<Either<ErrorResult,IBuildingDTO>> {
        try {
            /* TODO: error check */
            const code = BuildingCode.create(dto.code).getValue()
            const name = BuildingName.create(dto.name).getValue()
            const description = BuildingDescription.create(dto.description ?? '' /* FIXME */).getValue()

            const { length, width } = dto.maxFloorDimensions
            const maxFloorDimensions = MaxFloorDimensions.create(length, width).getValue()

            const result = Building.create({ code, name, description, maxFloorDimensions })
            if (result.isFailure) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: "Building parameters do not meet requirements"
                })
            }

            const building = result.getValue()
            await this.buildingRepo.save(building)

            return right(BuildingMap.toDTO(building))
        } catch (e) {
            return left({
                errorCode: ErrorCode.BussinessRuleViolation,
                message: "Error businessRuleViolation"
            })
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

    public async getBuildings(): Promise<Result<IBuildingDTO[]>> {
        try {
            const buildings = await this.buildingRepo.findAll();

            if (buildings.length === 0) {
                return Result.fail('Buildings not found');
            } else {
                const dtoList = await Promise.all(buildings.map(building => BuildingMap.toDTO(building)));
                return Result.ok(dtoList);
            }
        } catch (e) {
            throw e
        }
    }

    public async getBuildingsByFloors(dto: IBuildingMinMaxFloorsDTO): Promise<Result<IBuildingFloorNumberDTO[]>> {
        try {
            const buildingsAndFloorCount = await this.floorRepo.findBuildingsByMinMaxFloors(dto.minMaxFloors.min, dto.minMaxFloors.max);

            if (buildingsAndFloorCount.length == 0) {
                return Result.fail('Buildings not found')
            }

            const dtoList = await Promise.all(
                buildingsAndFloorCount.map(async (value) => {
                    const building = await this.buildingRepo.findByCode(value.buildingCode);
                    return BuildingFloorNumberMap.toDTO(building, value.floorCount);
                })
            );

            return Result.ok(dtoList)
        } catch (e) {
            throw e
        }
    }


    public async editBuilding(code: string, dto: IBuildingEditDTO): Promise<Either<ErrorResult,IBuildingDTO>> {
        try {
            const bCode = BuildingCode.create(code)
            if (bCode.isFailure) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building Code does not meet requirements" 
                    }as ErrorResult)
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())
            if (building === null) {
                return left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building not found" 
                }as ErrorResult)
            }

            //TODO: optimize
            if(dto.name){
                building.name = BuildingName.create(dto.name).getValue();
            }
            if(dto.description){
                building.description = BuildingDescription.create(dto.description).getValue();
            }

            if(dto.maxFloorDimensions){
                let length;
                let  width;

                if(dto.maxFloorDimensions.length){
                    length = dto.maxFloorDimensions.length
                }else{
                    length = building.maxFloorDimensions.length
                }

                if(dto.maxFloorDimensions.width){
                    width = dto.maxFloorDimensions.width
                }else{
                    width = building.maxFloorDimensions.width
                }

                building.maxFloorDimensions = MaxFloorDimensions.create(length, width).getValue()
            }

            const buildingRes = await this.buildingRepo.save(building)

            return right(BuildingMap.toDTO(buildingRes))

        } catch (e) {
            throw e
        }
    }
}
