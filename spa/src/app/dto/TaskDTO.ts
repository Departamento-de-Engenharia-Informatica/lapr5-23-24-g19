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
    PLANNED = 'Planned',
}

export enum TaskType {
    SURVEILLANCE = 'Surveillance',
    DELIVERY = 'Delivery',
}

export interface TaskDTO {
    id: string
    state: TaskState
    type: TaskType

    requesterEmail: string
    requesterName: string

    location: {
        startingPoint: {
            buildingCode: string
            floorNumber: number
            x: number
            y: number
        }
        endingPoint: {
            buildingCode: string
            floorNumber: number
            x: number
            y: number
        }
    }

    surveillanceContact?: {
        name: string
        phoneNumber: string
    }

    pickupContact?: {
        name: string
        phoneNumber: string
    }
    deliveryContact?: {
        name: string
        phoneNumber: string
    }
    confirmationCode?: {
        code: number
    }
    description?: string
}
