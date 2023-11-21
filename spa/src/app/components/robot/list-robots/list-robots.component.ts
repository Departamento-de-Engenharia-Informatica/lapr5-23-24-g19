import { Component, OnInit } from '@angular/core'
import { RobotDTO, RobotService } from 'src/app/services/robot.service'

@Component({
    selector: 'app-list-robots',
    templateUrl: './list-robots.component.html',
    styleUrls: ['./list-robots.component.css'],
})
export class ListRobotsComponent implements OnInit {
    robots: RobotDTO[]

    constructor(private robotService: RobotService) {
        this.robots = []
    }

    ngOnInit(): void {
        this.robotService.getRobots().subscribe((list: RobotDTO[]) => {
            this.robots = list
        })
    }
}
