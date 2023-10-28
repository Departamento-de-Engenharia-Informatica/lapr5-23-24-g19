import config from '../../config'
import { Service, Inject } from 'typedi'
import { Either, left,right } from '../core/logic/Result'

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
import IFloorRepo from './IRepos/IFloorRepo'
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

            
            if(await this.buildingRepo.findByCode(code)!=null){
                return left({
                    errorCode: ErrorCode.AlreadyExists,
                    message: "Building already exists"
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

    public async getBuilding(code: string): Promise<Either<ErrorResult,IBuildingDTO>> {
        try {
            const bCode = BuildingCode.create(code)
            if (bCode.isFailure) {
                return  left({
                    errorCode: ErrorCode.BussinessRuleViolation,
                    message: "Building code not valid"
                })
            }

            const building = await this.buildingRepo.findByCode(bCode.getValue())
            if (building === null) {
                return  left({
                    errorCode: ErrorCode.NotFound,
                    message: "Building not found"
                })
            } else {
                return right(BuildingMap.toDTO(building))
            }
        } catch (e) {
            throw e
        }
    }

    public async getBuildings(): Promise<Either<ErrorResult,IBuildingDTO[]>> {
        try {
            const buildings = await this.buildingRepo.findAll();

            if (buildings.length === 0) {
                return  left({
                    errorCode: ErrorCode.NotFound,
                    message: "Buildings not found"
                })
            } else {
                const dtoList = await Promise.all(buildings.map(building => BuildingMap.toDTO(building)));
                return right(dtoList)
            }
        } catch (e) {
            throw e
        }
    }

    public async getBuildingsByFloors(dto: IBuildingMinMaxFloorsDTO): Promise<Either<ErrorResult,IBuildingDTO[]>> {
        try {
            const buildingsAndFloorCount = await this.floorRepo.findBuildingsByMinMaxFloors(dto.minMaxFloors.min, dto.minMaxFloors.max);

            if (buildingsAndFloorCount.length == 0) {
                return  left({
                    errorCode: ErrorCode.NotFound,
                    message: "Buildings not found"
                })
            }

            const dtoList = await Promise.all(
                buildingsAndFloorCount.map(async (value) => {
                    const building = await this.buildingRepo.findByCode(value.buildingCode);
                    return BuildingFloorNumberMap.toDTO(building, value.floorCount);
                })
            );

            return right(dtoList)
        } catch (e) {
            throw e
        }
    }
    
    public async editBuilding(dto: IBuildingEditDTO): Promise<Either<ErrorResult,IBuildingDTO>> {
        try {
            const bCode = BuildingCode.create(dto.code)
            if (bCode.isFailure) {
                return left({
                    errorCode: ErrorCode.BussinessRuleViolation,
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

            if(dto.name){
                const nameEdit = BuildingName.create(dto.name);
                if (nameEdit.isFailure) {
                    return left({
                        errorCode: ErrorCode.BussinessRuleViolation,
                        message: "Building Name does not meet requirements" 
                    }as ErrorResult)
                }
                building.name = nameEdit.getValue()
                
            }
            if(dto.description){
                const descrEdit = BuildingDescription.create(dto.description)
                if (descrEdit.isFailure) {
                    return left({
                        errorCode: ErrorCode.BussinessRuleViolation,
                        message: "Building Descr does not meet requirements" 
                    }as ErrorResult)
                }
                building.description = descrEdit.getValue()
            }

            if(dto.maxFloorDimensions){
                const length = dto.maxFloorDimensions.length
                const width = dto.maxFloorDimensions.width

                const maxFloor = MaxFloorDimensions.create(length, width)
                if (maxFloor.isFailure) {
                    return left({
                        errorCode: ErrorCode.BussinessRuleViolation,
                        message: "Building Dimensios does not meet requirements" 
                    }as ErrorResult)
                }
                building.maxFloorDimensions = maxFloor.getValue()
            }

            const buildingRes = await this.buildingRepo.save(building)

            return right(BuildingMap.toDTO(buildingRes))

        } catch (e) {
            return left({
                errorCode: ErrorCode.BussinessRuleViolation,
                message: "Business rule violation" 
            }as ErrorResult)
        }
    }
}
