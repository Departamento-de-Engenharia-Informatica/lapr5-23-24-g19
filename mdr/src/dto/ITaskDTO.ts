export interface ITaskDTO {
    Email: string
    Location: {
        StartingPoint: {
            BuildingCode: string
            FloorNumber: number
            X: number
            Y: number
        }
        EndingPoint: {
            BuildingCode: string
            FloorNumber: number
            X: number
            Y: number
        }
    }

    JobType: number

    Delivery?: {
        PickupContact: {
            Name: string
            Phone: string
        }
        DeliveryContact: {
            Name: string
            Phone: string
        }
        Description: string
        ConfirmationCode: number
    }

    Surveillance?: {
        Contact: {
            Name: string
            Phone: string
        }
    }
}
