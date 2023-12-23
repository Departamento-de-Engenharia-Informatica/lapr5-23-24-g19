//TODO: should use DomainID's

// export interface RouteDTO{
//     room1:{
//         buildingCode: String
//         floorNumber: number
//         room: string
//     }
//     room2:{
//         buildingCode: String
//         floorNumber: number
//         room: string
//     }
// }

export enum TaskState {
    PENDING = 'Pending',
    APPROVED = 'Approved',
    REJECTED = 'Rejected',
}

export enum TaskType {
    DELIVERY = 'Delivery',
    SURVEILLANCE = 'Surveillance',
}


export interface TaskDTO {
    // NOTE: add fields as needed

    id: string
    state: TaskState
    type: TaskType

    requesterEmail: string
    requesterName: string
}
