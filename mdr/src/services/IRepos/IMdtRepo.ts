import { IFilterDTO } from '../../dto/IFilterDTO'
import { ITaskDTO } from '../../dto/ITaskDTO'
import { IUpdateTaskDTO } from '../../dto/IUpdateTaskDTO'

export default interface IMdtAdapter {
    createSurveillanceTask(dto: ITaskDTO): Promise<String>
    getByFilter(dto: IFilterDTO): Promise<String>
    getByStatus(status: string): Promise<String>
    createDeliveryTask(dto: ITaskDTO): Promise<String>
    updateTask(dto: IUpdateTaskDTO): Promise<string>
}
