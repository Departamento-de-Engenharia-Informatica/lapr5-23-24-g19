import { Mapper } from '../core/infra/Mapper'
import { IFloorDTO } from '../dto/IFloorDTO'
import { Floor } from '../domain/floor/floor'
import { FloorNumber } from '../domain/floor/floorNumber'
import { BuildingCode } from '../domain/building/buildingCode'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { IFloorPersistence } from '../dataschema/IFloorPersistence'
import Container from 'typedi'
import BuildingRepo from '../repos/buildingRepo'
import { Description } from '../domain/description'
import { FloorMapContent } from '../domain/floor/floorMap'
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { Coordinates } from '../domain/floor/Coordinates'

export class FloorMap extends Mapper<Floor> {
    public static toDTO(Floor: Floor): IFloorDTO {
        return {
            buildingCode: Floor.building.code.value,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description.value,
        } as IFloorDTO
    }

    public static async toDomain(raw: any): Promise<Floor> {
        //TODO: implement map component toDomain
        const buildingRepo = Container.get(BuildingRepo)
        const building = await buildingRepo.findByCode(BuildingCode.create(raw.buildingCode).getValue())
        const FloorOrError = Floor.create(
            {
                floorNumber: FloorNumber.create(raw.floorNumber).getValue(),
                description: Description.create(raw.description).getValue(),
                building: building,
            },
            new UniqueEntityID(raw.domainId)
        )

        if (FloorOrError.isFailure) {
            console.log(FloorOrError.error)
            return null
        }

        return FloorOrError.isSuccess ? FloorOrError.getValue() : null
    }

    public static toPersistence(Floor: Floor): IFloorPersistence {
        const map = Floor.props.map;
        return {
            domainId: Floor.id.toString(),
            buildingCode: Floor.building.code.value,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description.value,
            map:{
                dimensions:{
                    mapLength: map.dimensions.length,
                    mapWidth: map.dimensions.width
                },
                mapContent: map.mapContent,
                passages: map.passages.map(passage =>{return {x:passage.x,y: passage.y}}),
                elevators: map.elevators.map(elevator =>{return {x:elevator.x,y: elevator.y}}),
                rooms: map.rooms.map(room =>{return {x:room.x,y: room.y}}),
            }
        }
    }

    public static toDTOFloorMap(floor: Floor): IFloorMapDTO {
        const map = floor.props.map
        return {
            buildingCode: floor.building.code.value,
            floorNumber: floor.floorNumber.value,
            dimensions:{
                length: floor.props.map.dimensions.length,
                width: floor.props.map.dimensions.width
            },
            mapContent: map.mapContent,
            passages: map.passages.map(passage =>{return {x:passage.x,y: passage.y}}),
            elevators: map.elevators.map(elevator =>{return {x:elevator.x,y: elevator.y}}),
            rooms: map.rooms.map(room =>{return {x:room.x,y: room.y}}),
      
        } as IFloorMapDTO
    }
}
