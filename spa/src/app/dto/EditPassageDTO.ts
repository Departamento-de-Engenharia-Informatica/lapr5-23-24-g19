import { PassageDTO } from "./PassageDTO"

export interface EditPassageDTO {
    old: PassageDTO
    new: Partial<PassageDTO>
}
