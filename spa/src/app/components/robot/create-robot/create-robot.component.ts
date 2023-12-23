import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { CreateRobotTypeDTO } from 'src/app/dto/CreateRobotTypeDTO'
import { RobotDTO } from 'src/app/dto/RobotDTO'
import { RobotWithoutStateDTO } from 'src/app/dto/RobotWithoutStateDTO'
import { RobotService } from 'src/app/services/robot.service'

@Component({
    selector: 'app-create-robot',
    templateUrl: './create-robot.component.html',
    styleUrls: ['./create-robot.component.css'],
})
export class CreateRobotComponent implements OnInit {
    selectedRobotType: string = ''
    createRobotForm: FormGroup = null as unknown as FormGroup

    robotTypes: CreateRobotTypeDTO[] = []
    robots: RobotDTO[] = []

    constructor(private fb: FormBuilder, private svc: RobotService) {
        this.createRobotForm = this.fb.group({
            code: [null, [Validators.required]],
            nickname: [null, [Validators.required]],
            typeCode: [null, [Validators.required]],
            serialNumber: [null, [Validators.required]],
            description: '',
        })
    }

    ngOnInit(): void {
        this.getRobots()

        this.svc.getRobotTypes().subscribe(
            (list: CreateRobotTypeDTO[]) => {
                this.robotTypes = list
            },
            (error) => {
                alert(error.error)

                this.robotTypes = []

                this.createRobotForm.reset({
                    description: '',
                })

                this.getRobots()
            },
        )
    }

    getRobots(): void {
        this.svc.getRobotsOnion().subscribe((list: RobotDTO[]) => {
            this.robots = list
        })
    }

    onSubmit(): void {
        const dto: RobotWithoutStateDTO = {
            code: this.createRobotForm.value.code,
            nickname: this.createRobotForm.value.nickname,
            typeCode: this.createRobotForm.value.typeCode,
            serialNumber: this.createRobotForm.value.serialNumber,
        }

        const description = this.createRobotForm.value.description
        if (description !== '') dto.description = description

        this.svc.createRobot(dto).subscribe(
            (robot: RobotDTO) => {
                let alertMessage = `Robot created successfully!\nCode: ${robot.code}\nNickname: ${robot.nickname}\nRobot Type code: ${robot.typeCode}\nSerial Number: ${robot.serialNumber}`

                if (robot.description) {
                    alertMessage += `\nDescription: ${robot.description}`
                }

                alert(alertMessage)

                this.createRobotForm.reset({
                    description: '',
                })

                this.getRobots()
            },
            (error) => {
                alert(error.error)
                this.createRobotForm.reset({
                    description: '',
                })
            },
        )
    }
}
