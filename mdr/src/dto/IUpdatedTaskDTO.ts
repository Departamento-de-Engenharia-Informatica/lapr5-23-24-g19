enum TaskState {
    PENDING = 'Pending',
    APPROVED = 'Approved',
    REJECTED = 'Rejected',
}

enum TaskType {
    SURVEILLANCE = 'Surveillance',
    DELIVERY = 'Delivery',
}

export interface IUpdatedTaskDTO {
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
