import { CommentDto } from '@core/eru/dtos/comment.dto';

export class PostDto
{
  Id: string;
  UserId: string;
  StatusId: number;
  CategoryId: number;
  CreateTime: string;
  UpdateTime: string;
  Content: string;
  Title: string;
  Description: string;
  User?: {
    Avatar: string
    Name: string,
    Id: string,
    Description: string,
    Url: string
  };
  Comments: CommentDto[];
}
