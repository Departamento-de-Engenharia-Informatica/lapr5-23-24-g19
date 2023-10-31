export interface IFloorMapDTO {
    buildingCode: string
    floorNumber: number,
    dimensions: {length: number; width: number} 
    mapContent: number[][]
    passages: {x:number; y:number} []
    elevators: {x:number; y:number} []
    rooms: {x:number; y:number} []
}
