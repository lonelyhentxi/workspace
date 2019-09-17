import { Component, OnInit } from '@angular/core';
import { _HttpClient } from '@delon/theme';
import { switchMap } from 'rxjs/operators';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { of } from 'rxjs';
import { ApiService } from '@core/eru/api.service';
import { NzNotificationService } from 'ng-zorro-antd';
import { PostDto } from '@core/eru/dtos/post.dto';
import { HttpParams } from '@angular/common/http';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
})
export class DashboardComponent implements OnInit {
  $routeSubscription;
  categorieId: number = null;
  posts: PostDto[] = [];

  constructor(
    private http: _HttpClient,
    private route: ActivatedRoute,
    private router: Router,
    private apiService: ApiService,
    private notification: NzNotificationService,
  ) {
  }

  loadPost(id: number) {
    this.posts.splice(0);
    let params = {};
    if(id) {
      params = {CategoryId: id};
    }
    this.http.get(this.apiService.apiJoin(`posts`),params)
      .subscribe((postRes) => {
        const posts = (postRes as any).body as PostDto[];
        this.posts.splice(0, 0, ...posts);
      }, (error) => {
        const errorMessage: string = (error as any).message;
        this.notification.error('错误', errorMessage);
      });
  }

  async ngOnInit() {
    this.$routeSubscription = this.route.params.subscribe(async (params) => {
      this.categorieId = params.id;
      await this.loadPost(params.id);
    });
  }

}
