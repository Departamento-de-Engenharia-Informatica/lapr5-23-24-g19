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
import { IFloorMapDTO } from '../dto/IFloorMapDTO'
import { FloorMapContent, FloorMapProps } from '../domain/floor/floorMap'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'
import { Coordinates } from '../domain/floor/Coordinates'

export class FloorMap extends Mapper<Floor> {
    public static toDTO(Floor: Floor): IFloorDTO {
        return {
            buildingCode: Floor.building.code.value,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description?.value,
        } as IFloorDTO
    }

    public static async toDomain(raw: any): Promise<Floor> {
        //TODO: implement map component toDomain
        const buildingRepo = Container.get(BuildingRepo)
        const building = await buildingRepo.findByCode(BuildingCode.create(raw.buildingCode).getValue())
        const description = raw.description ? Description.create(raw.description).getValue() : undefined;
        let map = undefined
        if(!!raw.map){
            const mapOrError  = FloorMapContent.create({
                dimensions: MaxFloorDimensions.create(raw.map.dimensions.length,raw.map.dimensions.width).getValue(),
                mapContent: raw.map.mapContent,
                passages: raw.map.passages.map(passage => {return Coordinates.create(passage.x,passage.y)}),
                elevators: raw.map.elevators.map(elevator => {return Coordinates.create(elevator.x,elevator.y)}),
                rooms: raw.map.rooms.map(room => {return Coordinates.create(room.x,room.y)}),
            }as FloorMapProps)
            
            if (mapOrError.isFailure) {
                return null
            }
            map = mapOrError.getValue()
        }

        const FloorOrError = Floor.create(
            {
                floorNumber: FloorNumber.create(raw.floorNumber).getValue(),
                description: description,
                building: building,
                map: map
            },
            new UniqueEntityID(raw.domainId),
        )

        if (FloorOrError.isFailure) {
            return null
        }

        return FloorOrError.isSuccess ? FloorOrError.getValue() : null
    }

    public static toPersistence(floor: Floor): IFloorPersistence {
        const map = floor.props.map
        const pers= {
            domainId: floor.id.toString(),
            buildingCode: floor.building.code.value,
            floorNumber: floor.floorNumber.value,
            description: floor.description?.value,
        }as IFloorPersistence

        if(map!=null && map != undefined){
            
            pers.map = {
                dimensions:{
                    length: map?.dimensions.length,
                    width: map?.dimensions.width,
                },
            mapContent: map?.mapContent,
            passages: map?.passages.map((passage) => {
                return { x: passage.x, y: passage.y }
            }),
            elevators: map?.elevators.map((elevator) => {
                return { x: elevator.x, y: elevator.y }
            }),
            rooms: map?.rooms.map((room) => {
                return { x: room.x, y: room.y }
            }),
            }
        }
        return pers
    }

    public static toDTOFloorMap(floor: Floor): IFloorMapDTO {
        const map = floor.props.map
        return{
            buildingCode: floor.building.code.value,
            floorNumber: floor.floorNumber.value,
            dimensions: {
                length: map.props.dimensions.length,
                width: map.props.dimensions.width,
            },
            mapContent: map.mapContent,
            passages: map.passages.map((passage) => {
                return { x: passage.x, y: passage.y }
            }),
            elevators: map.elevators.map((elevator) => {
                return { x: elevator.x, y: elevator.y }
            }),
            rooms: map.rooms.map((room) => {
                return { x: room.x, y: room.y }
            }),
        } as IFloorMapDTO
    }
}
