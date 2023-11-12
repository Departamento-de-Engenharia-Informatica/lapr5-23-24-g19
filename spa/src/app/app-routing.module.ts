import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BuildingComponent } from './components/building/building.component';
import { RobotComponent } from './components/robot/robot.component';
import { FloorComponent } from './components/floor/floor.component';
import { PassageComponent } from './components/passage/passage.component';
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component';
import { RobotTypeComponent } from './components/robot-type/robot-type.component';
import { ElevatorComponent } from './components/elevator/elevator.component';
import { RoomComponent } from './components/room/room.component';
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component';
import { AppComponent } from './app.component';
import { MainMenuComponent } from './components/main-menu/main-menu.component';
import { AuthComponent } from './components/auth/auth.component';
import { ListFloorsComponent } from './components/floor/list-floors/list-floors.component';

export const routes: Routes = [
    { path: '', component: AuthComponent, title: 'Auth page' },
    { path: 'menu', component: MainMenuComponent, title: 'Home page' },
    { path: 'buildings', component: BuildingComponent, title: 'Buildings' },
    { path: 'floors', component: FloorComponent },
    { path: 'list-floors', component: ListFloorsComponent },
    { path: 'elevators', component: ElevatorComponent },
    { path: 'rooms', component: RoomComponent },
    { path: 'passages', component: PassageComponent },
    { path: 'robot-types', component: RobotTypeComponent },
    { path: 'robots', component: RobotComponent },
    { path: '3D-visualization', component: Visualization3DComponent },
    { path: '**', component: PageNotFoundComponent },
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule {}
